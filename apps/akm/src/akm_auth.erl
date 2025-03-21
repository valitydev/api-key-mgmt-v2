-module(akm_auth).

-define(APP, akm).

-include_lib("bouncer_proto/include/bouncer_ctx_thrift.hrl").

-export([get_subject_id/1]).
-export([get_party_id/1]).
-export([get_user_id/1]).
-export([get_user_email/1]).
-export([extract_auth_context/1]).

-export([preauthorize_api_key/1]).
-export([authorize_api_key/3]).
-export([authorize_operation/2]).

-export([put_party_to_metadata/1]).
-export([put_party_to_metadata/2]).
-export([get_party_from_metadata/1]).

-export_type([resolution/0]).
-export_type([preauth_context/0]).
-export_type([auth_context/0]).
-export_type([api_key/0]).

%%

-type token_type() :: bearer.
-type auth_context() :: {authorized, token_keeper_client:auth_data()}.
-type preauth_context() :: {unauthorized, {token_type(), token_keeper_client:token()}}.
-type api_key() :: binary().

-type resolution() ::
    allowed
    | forbidden
    | {forbidden, _Reason}.

-define(AUTHORIZED(Ctx), {authorized, Ctx}).
-define(UNAUTHORIZED(Ctx), {unauthorized, Ctx}).

%%

-spec get_subject_id(auth_context()) -> binary() | undefined.
get_subject_id(AuthContext) ->
    case get_party_id(AuthContext) of
        PartyId when is_binary(PartyId) ->
            PartyId;
        undefined ->
            get_user_id(AuthContext)
    end.

-spec get_party_id(auth_context()) -> binary() | undefined.
get_party_id(?AUTHORIZED(#{metadata := Metadata})) ->
    get_metadata(get_metadata_mapped_key(party_id), Metadata).

-spec get_user_id(auth_context()) -> binary() | undefined.
get_user_id(?AUTHORIZED(#{metadata := Metadata})) ->
    get_metadata(get_metadata_mapped_key(user_id), Metadata).

-spec get_user_email(auth_context()) -> binary() | undefined.
get_user_email(?AUTHORIZED(#{metadata := Metadata})) ->
    get_metadata(get_metadata_mapped_key(user_email), Metadata).
%%

-spec preauthorize_api_key(api_key()) -> {ok, preauth_context()} | {error, _Reason}.
preauthorize_api_key(ApiKey) ->
    case parse_api_key(ApiKey) of
        {ok, Token} ->
            {ok, ?UNAUTHORIZED(Token)};
        {error, Error} ->
            {error, Error}
    end.

-spec authorize_api_key(preauth_context(), token_keeper_client:token_context(), woody_context:ctx()) ->
    {ok, auth_context()} | {error, _Reason}.
authorize_api_key(?UNAUTHORIZED({TokenType, Token}), TokenContext, WoodyContext) ->
    authorize_token_by_type(TokenType, Token, TokenContext, WoodyContext).

authorize_token_by_type(bearer, Token, TokenContext, WoodyContext) ->
    Authenticator = token_keeper_client:authenticator(WoodyContext),
    case token_keeper_authenticator:authenticate(Token, TokenContext, Authenticator) of
        {ok, AuthData} ->
            {ok, ?AUTHORIZED(AuthData)};
        {error, TokenKeeperError} ->
            _ = logger:warning("Token keeper authorization failed: ~p", [TokenKeeperError]),
            {error, {auth_failed, TokenKeeperError}}
    end.

-spec authorize_operation(
    Prototypes :: akm_bouncer_context:prototypes(),
    Context :: akm_apikeys_handler:handler_context()
) -> resolution().
authorize_operation(Prototypes, Context) ->
    AuthContext = extract_auth_context(Context),
    #{swagger_context := SwagContext, woody_context := WoodyContext} = Context,
    IPAddress = get_ip_address(SwagContext),
    Fragments = akm_bouncer:gather_context_fragments(
        get_token_keeper_fragment(AuthContext),
        get_user_id(AuthContext),
        IPAddress,
        WoodyContext
    ),
    Fragments1 = akm_bouncer_context:build(Prototypes, Fragments),
    akm_bouncer:judge(Fragments1, WoodyContext).

%%

get_token_keeper_fragment(?AUTHORIZED(#{context := Context})) ->
    Context.

-spec extract_auth_context(akm_apikeys_handler:handler_context()) ->
    akm_apikeys_handler:auth_context().
extract_auth_context(#{swagger_context := #{auth_context := AuthContext}}) ->
    AuthContext.

parse_api_key(<<"Bearer ", Token/binary>>) ->
    {ok, {bearer, Token}};
parse_api_key(_) ->
    {error, unsupported_auth_scheme}.

%%
-spec put_party_to_metadata(binary()) -> map().
put_party_to_metadata(PartyId) ->
    put_party_to_metadata(PartyId, #{}).

-spec put_party_to_metadata(binary(), map()) -> map().
put_party_to_metadata(PartyId, Metadata) ->
    put_metadata(get_metadata_mapped_key(party_id), PartyId, Metadata).

-spec get_party_from_metadata(map()) -> binary() | undefined.
get_party_from_metadata(Metadata) ->
    get_metadata(get_metadata_mapped_key(party_id), Metadata).

get_metadata(Key, Metadata) ->
    maps:get(Key, Metadata, undefined).

put_metadata(Key, Value, Metadata) ->
    maps:put(Key, Value, Metadata).

get_metadata_mapped_key(Key) ->
    maps:get(Key, get_meta_mappings()).

get_meta_mappings() ->
    AuthConfig = genlib_app:env(?APP, auth_config),
    maps:get(metadata_mappings, AuthConfig).

get_ip_address(SwagContext) ->
    Request = maps:get(cowboy_req, SwagContext, #{}),
    case get_ip_address_from_request(Request) of
        {ok, IPAddress} ->
            IPAddress;
        {error, _Error} ->
            %% Ignore error, add logging if needed
            undefined
    end.

get_ip_address_from_request(Request) ->
    IPAddressHeader = genlib_app:env(akm, ip_address_header, <<"x-forwarded-for">>),
    case Request of
        #{headers := #{IPAddressHeader := IPAddress}} ->
            parse_header_ip_address(IPAddress);
        #{peer := {IPAddress, _Port}} ->
            {ok, IPAddress};
        _ ->
            {error, no_req_in_swag_context}
    end.

parse_header_ip_address(IPAddress0) ->
    IPAddress1 = erlang:binary_to_list(IPAddress0),
    IPs = [L || L <- string:lexemes(IPAddress1, ", ")],
    Valid = lists:all(fun check_ip/1, IPs),
    case IPs of
        [ClientIP | _Proxies] when Valid ->
            inet:parse_strict_address(ClientIP);
        _ ->
            % empty or malformed value
            {error, malformed}
    end.

check_ip(IP) ->
    case inet:parse_strict_address(IP) of
        {ok, _} ->
            true;
        _Error ->
            % unparseable ip address
            false
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec determine_peer_test_() -> [_TestGen].
determine_peer_test_() ->
    [
        ?_assertEqual(
            {ok, {10, 10, 10, 10}},
            parse_header_ip_address(<<"10.10.10.10">>)
        ),
        ?_assertEqual(
            {ok, {17, 71, 0, 1}},
            parse_header_ip_address(<<"17.71.0.1">>)
        ),
        ?_assertEqual(
            {ok, {17, 71, 0, 1}},
            parse_header_ip_address(<<" 17.71.0.1,123.123.123.123 ">>)
        ),
        ?_assertEqual(
            {error, malformed},
            parse_header_ip_address(<<",,,,">>)
        ),
        ?_assertEqual(
            {ok, {1, 1, 1, 1}},
            parse_header_ip_address(<<"1.1.1.1,,, ,,,">>)
        ),
        ?_assertEqual(
            {error, malformed},
            parse_header_ip_address(<<"1.,1.,1.1,">>)
        )
    ].

-spec metadata_test() -> _.
metadata_test() ->
    application:set_env(
        akm,
        auth_config,
        #{metadata_mappings => #{party_id => <<"dev.vality.party.id">>}}
    ),
    ?assertEqual(#{<<"dev.vality.party.id">> => <<"qqq">>}, put_party_to_metadata(<<"qqq">>)),
    ?assertEqual(
        #{<<"dev.vality.party.id">> => <<"qqq">>, 1 => 2},
        put_party_to_metadata(<<"qqq">>, #{1 => 2})
    ),
    ?assertEqual(<<"qqq">>, get_party_from_metadata(#{<<"dev.vality.party.id">> => <<"qqq">>})).

-endif.
