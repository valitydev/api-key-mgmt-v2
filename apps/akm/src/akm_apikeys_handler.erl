-module(akm_apikeys_handler).

-export([prepare/4]).

%% Types

-type request_data() :: #{atom() | binary() => term()}.
-type status_code() :: 200..599.
-type headers() :: cowboy:http_headers().
-type response_data() :: map() | [map()] | undefined.
-type response() :: {status_code(), headers(), response_data()}.
-type request_result() :: {ok | error, response()}.
-type request_state() :: #{
    authorize := fun(() -> {ok, akm__auth:resolution()} | request_result()),
    process := fun(() -> request_result())
}.

-type operation_id() :: atom().
-type swag_schema() :: map().
-type operation_spec() :: map().
-type swag_server_get_schema_fun() :: fun(() -> swag_schema()).
-type swag_server_get_operation_fun() :: fun((operation_id()) -> operation_spec()).

-type client_peer() :: #{
    ip_address => IP :: inet:ip_address(),
    port_number => Port :: inet:port_number()
}.
-type auth_context() :: any().
-type req() :: cowboy_req:req().
-type request_context() :: #{
    auth_context => AuthContext :: auth_context(),
    peer => client_peer(),
    cowboy_req => req()
}.

-type handler_opts() :: _.
-type handler_context() :: #{
    operation_id := operation_id(),
    woody_context := woody_context:ctx(),
    swagger_context := request_context(),
    swag_server_get_schema_fun := swag_server_get_schema_fun(),
    swag_server_get_operation_fun := swag_server_get_operation_fun()
}.

-export_type([request_data/0]).
-export_type([request_result/0]).

-export_type([handler_opts/0]).
-export_type([status_code/0]).
-export_type([headers/0]).
-export_type([response_data/0]).
-export_type([request_context/0]).
-export_type([operation_id/0]).
-export_type([handler_context/0]).
-export_type([swag_server_get_schema_fun/0]).
-export_type([swag_server_get_operation_fun/0]).

respond_if_forbidden(forbidden, Response) ->
    Response;
respond_if_forbidden(allowed, _Response) ->
    allowed.

mask_notfound(Resolution) ->
    % ED-206
    % When bouncer says "forbidden" we can't really tell the difference between "forbidden because
    % of no such invoice", "forbidden because client has no access to it" and "forbidden because
    % client has no permission to act on it". From the point of view of existing integrations this
    % is not great, so we have to mask specific instances of missing authorization as if specified
    % invoice is nonexistent.
    respond_if_forbidden(Resolution, akm_handler_utils:reply_ok(404)).

%% Providers
-spec prepare(operation_id(), request_data(), handler_context(), handler_opts()) -> {ok, request_state()}.
prepare(OperationID = 'ListProviders', #{'residence' := Residence}, Context, _Opts) ->
    Authorize = fun() ->
        Prototypes = [{operation, #{id => OperationID}}],
        Resolution = akm_auth:authorize_operation(Prototypes, Context),
        {ok, Resolution}
    end,
    Process = fun() ->
        Providers = akm_provider_backend:get_providers(maybe_to_list(Residence), Context),
        akm_handler_utils:reply_ok(200, Providers)
    end,
    {ok, #{authorize => Authorize, process => Process}}
.

%% Internal functions
get_location(OperationId, Params, #{swag_server_get_operation_fun := Get}, Opts) ->
    #{path := PathSpec} = Get(OperationId),
    akm_handler_utils:get_location(PathSpec, Params, Opts).

issue_grant_token(TokenSpec, Expiration, Context) ->
    case get_expiration_deadline(Expiration) of
        {ok, Deadline} ->
            {ok, akm_tokens_legacy:issue_access_token(akm_handler_utils:get_owner(Context), TokenSpec, Deadline)};
        Error = {error, _} ->
            Error
    end.

get_expiration_deadline(Expiration) ->
    {DateTime, MilliSec} = woody_deadline:from_binary(akm_utils:to_universal_time(Expiration)),
    Deadline = genlib_time:daytime_to_unixtime(DateTime) + MilliSec div 1000,
    case genlib_time:unow() - Deadline < 0 of
        true ->
            {ok, Deadline};
        false ->
            {error, expired}
    end.

build_auth_context([], Acc, _Context) ->
    Acc;
build_auth_context([undefined | T], Acc, Context) ->
    build_auth_context(T, Acc, Context);
build_auth_context([H | T], Acc, Context) ->
    AuthContext = build_auth_context(H, Context),
    build_auth_context(T, [AuthContext | Acc], Context).

build_auth_context({identity, IdentityID}, Context) ->
    {ResultIdentity, ResultIdentityOwner} =
        case akm_identity_backend:get_identity(IdentityID, Context) of
            {ok, Identity, Owner} -> {Identity, Owner};
            {error, {identity, notfound}} -> {undefined, undefined}
        end,
    {identity, {IdentityID, ResultIdentity, ResultIdentityOwner}};
build_auth_context({wallet, WalletID}, Context) ->
    {ResultWallet, ResultWalletOwner} =
        case akm_wallet_backend:get(WalletID, Context) of
            {ok, Wallet, Owner} -> {Wallet, Owner};
            {error, {wallet, notfound}} -> {undefined, undefined}
        end,
    {wallet, {WalletID, ResultWallet, ResultWalletOwner}};
build_auth_context({destination, DestinationID}, Context) ->
    {ResultDestination, ResultDestinationOwner} =
        case akm_destination_backend:get(DestinationID, Context) of
            {ok, Destination, Owner} -> {Destination, Owner};
            {error, {destination, notfound}} -> {undefined, undefined}
        end,
    {destination, {DestinationID, ResultDestination, ResultDestinationOwner}};
build_auth_context({withdrawal, WithdrawalId}, Context) ->
    {ResultWithdrawal, ResultWithdrawalOwner} =
        case akm_withdrawal_backend:get(WithdrawalId, Context) of
            {ok, Withdrawal, Owner} -> {Withdrawal, Owner};
            {error, {withdrawal, notfound}} -> {undefined, undefined}
        end,
    {withdrawal, {WithdrawalId, ResultWithdrawal, ResultWithdrawalOwner}};
build_auth_context({webhook, WebhookId}, Context) ->
    ResultWebhook =
        case akm_webhook_backend:get_webhook(WebhookId, Context) of
            {ok, Webhook} -> Webhook;
            {error, notfound} -> undefined
        end,
    {webhook, {WebhookId, ResultWebhook}}.

build_prototype_for(operation, OpContext, AuthContext) ->
    lists:foldl(
        fun
            ({identity, {IdentityID, _Identity, _Owner}}, Acc) ->
                Acc#{identity => IdentityID};
            ({wallet, {WalletID, _Wallet, _Owner}}, Acc) ->
                Acc#{wallet => WalletID};
            ({destination, {DestinationID, _Destination, _Owner}}, Acc) ->
                Acc#{destination => DestinationID};
            ({withdrawal, {WithdrawalID, _Withdrawal, _Owner}}, Acc) ->
                Acc#{withdrawal => WithdrawalID};
            ({webhook, {WebhookId, _ResultWebhook}}, Acc) ->
                Acc#{webhook => WebhookId}
        end,
        OpContext,
        AuthContext
    );
build_prototype_for(wallet, Entities, AuthContext) ->
    lists:foldl(
        fun
            ({identity, {_IdentityID, Identity, Owner}}, Acc) ->
                [akm_bouncer_context:build_wallet_entity(identity, Identity, {party, Owner}) | Acc];
            ({wallet, {_WalletID, Wallet, Owner}}, Acc) ->
                [akm_bouncer_context:build_wallet_entity(wallet, Wallet, {party, Owner}) | Acc];
            ({destination, {_DestinationID, Destination, Owner}}, Acc) ->
                [akm_bouncer_context:build_wallet_entity(destination, Destination, {party, Owner}) | Acc];
            ({withdrawal, {_WithdrawalID, Withdrawal, Owner}}, Acc) ->
                [akm_bouncer_context:build_wallet_entity(withdrawal, Withdrawal, {party, Owner}) | Acc];
            ({webhook, {_WebhookID, Webhook}}, Acc) ->
                [akm_bouncer_context:build_wallet_entity(webhook, Webhook) | Acc]
        end,
        Entities,
        AuthContext
    ).

find_party_id_for(Tag, [Context = {Tag, _} | _Rest]) -> get_party_id_from_auth_context(Context);
find_party_id_for(Tag, [_H | Rest]) -> find_party_id_for(Tag, Rest).

get_party_id_from_auth_context({identity, {_, _, PartyID}}) -> PartyID;
get_party_id_from_auth_context({wallet, {_, _, PartyID}}) -> PartyID.

add_party_id_to(undefined, Params) ->
    Params;
add_party_id_to(PartyID, Params) when is_map(Params) ->
    Params#{<<"partyID">> => PartyID}.

maybe_get_party_id_from_req(#{'partyID' := PartyID}, _Context) when PartyID =/= undefined ->
    PartyID;
maybe_get_party_id_from_req(_, Context) ->
    akm_handler_utils:get_owner(Context).

% seconds
-define(DEFAULT_URL_LIFETIME, 60).

get_default_url_lifetime() ->
    Now = erlang:system_time(second),
    Lifetime = application:get_env(akm_lib, file_storage_url_lifetime, ?DEFAULT_URL_LIFETIME),
    genlib_rfc3339:format(Now + Lifetime, second).

maybe_to_list(undefined) ->
    [];
maybe_to_list(T) ->
    [T].
