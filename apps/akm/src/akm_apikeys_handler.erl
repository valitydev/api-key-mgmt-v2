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
    authorize := fun(() -> {ok, akm_auth:resolution()} | request_result()),
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
-export_type([auth_context/0]).
-export_type([operation_id/0]).
-export_type([handler_context/0]).
-export_type([swag_server_get_schema_fun/0]).
-export_type([swag_server_get_operation_fun/0]).

%% Providers
-spec prepare(operation_id(), request_data(), handler_context(), handler_opts()) -> {ok, request_state()}.
prepare('IssueApiKey' = OperationID, #{'partyId' := PartyID, 'ApiKeyIssue' := ApiKey}, Context, _Opts) ->
    Authorize = fun() ->
        Prototypes = [{operation, #{id => OperationID, party => PartyID}}],
        Resolution = akm_auth:authorize_operation(Prototypes, Context),
        {ok, Resolution}
    end,
    Process = fun() ->
        #{woody_context := WoodyContext} = Context,
        case akm_apikeys_processing:issue_api_key(PartyID, ApiKey, WoodyContext) of
            {ok, Resp} ->
                akm_handler_utils:reply_ok(200, Resp);
            {error, already_exists} ->
                akm_handler_utils:reply_ok(400, #{
                    <<"errorType">> => <<"AlreadyExists">>,
                    <<"description">> => <<"This AccessToken already exists">>
                })
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('GetApiKey' = OperationID, #{'partyId' := PartyID, 'apiKeyId' := ApiKeyId}, Context, _Opts) ->
    Result = akm_apikeys_processing:get_api_key(ApiKeyId),
    Authorize = fun() ->
        ApiKey = extract_api_key(Result),
        Prototypes = [{operation, #{id => OperationID, party => PartyID, api_key => ApiKey}}],
        Resolution = akm_auth:authorize_operation(Prototypes, Context),
        {ok, Resolution}
    end,
    Process = fun() ->
        case Result of
            {ok, ApiKey} ->
                akm_handler_utils:reply_ok(200, ApiKey);
            {error, not_found} ->
                akm_handler_utils:reply_error(404)
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(
    'ListApiKeys' = OperationID,
    #{
        'partyId' := PartyID,
        'limit' := Limit,
        'status' := Status,
        continuationToken := ContinuationToken0
    },
    Context,
    _Opts
) ->
    Authorize = fun() ->
        Prototypes = [{operation, #{id => OperationID, party => PartyID}}],
        Resolution = akm_auth:authorize_operation(Prototypes, Context),
        {ok, Resolution}
    end,
    ContinuationToken = erlang:binary_to_integer(genlib:define(ContinuationToken0, <<"0">>)),
    Process = fun() ->
        {ok, Response} = akm_apikeys_processing:list_api_keys(PartyID, Status, Limit, ContinuationToken),
        akm_handler_utils:reply_ok(200, Response)
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare('RequestRevokeApiKey' = OperationID, Params, Context, _Opts) ->
    #{
        'partyId' := PartyID,
        'apiKeyId' := ApiKeyId,
        'RequestRevoke' := #{<<"status">> := Status}
    } = Params,
    Result = akm_apikeys_processing:get_api_key(ApiKeyId),
    Authorize = fun() ->
        ApiKey = extract_api_key(Result),
        Prototypes = [{operation, #{id => OperationID, party => PartyID, api_key => ApiKey}}],
        Resolution = akm_auth:authorize_operation(Prototypes, Context),
        {ok, Resolution}
    end,
    Process = fun() ->
        Email = akm_auth:get_user_email(akm_auth:extract_auth_context(Context)),
        case akm_apikeys_processing:request_revoke(Email, PartyID, ApiKeyId, Status) of
            {ok, revoke_email_sent} ->
                akm_handler_utils:reply_ok(204);
            {error, not_found} ->
                akm_handler_utils:reply_error(404)
        end
    end,
    {ok, #{authorize => Authorize, process => Process}};
prepare(
    'RevokeApiKey' = OperationID,
    #{'partyId' := PartyID, 'apiKeyId' := ApiKeyId, 'apiKeyRevokeToken' := Token},
    Context,
    _Opts
) ->
    Result = akm_apikeys_processing:get_api_key(ApiKeyId),
    Authorize = fun() ->
        ApiKey = extract_api_key(Result),
        Prototypes = [{operation, #{id => OperationID, party => PartyID, api_key => ApiKey}}],
        Resolution = akm_auth:authorize_operation(Prototypes, Context),
        {ok, Resolution}
    end,
    Process = fun() ->
        #{woody_context := WoodyContext} = Context,
        case akm_apikeys_processing:revoke(ApiKeyId, Token, WoodyContext) of
            ok ->
                akm_handler_utils:reply_ok(204);
            {error, not_found} ->
                akm_handler_utils:reply_error(404)
        end
    end,
    {ok, #{authorize => Authorize, process => Process}}.

extract_api_key({ok, ApiKey}) ->
    ApiKey;
extract_api_key(_) ->
    undefined.
