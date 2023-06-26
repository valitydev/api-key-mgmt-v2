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
-export_type([operation_id/0]).
-export_type([handler_context/0]).
-export_type([swag_server_get_schema_fun/0]).
-export_type([swag_server_get_operation_fun/0]).

%% Providers
-spec prepare(operation_id(), request_data(), handler_context(), handler_opts()) -> {ok, request_state()}.
prepare(OperationID = 'IssueApiKey', #{'partyId' := PartyID, 'ApiKeyIssue' := ApiKey}, Context, _Opts) ->
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
prepare(OperationID = 'GetApiKey', #{'partyId' := PartyID, 'apiKeyId' := ApiKeyId}, Context, _Opts) ->
    Authorize = fun() ->
        Prototypes = [{operation, #{id => OperationID, party => PartyID}}],
        Resolution = akm_auth:authorize_operation(Prototypes, Context),
        {ok, Resolution}
    end,
    Process = fun() ->
        case akm_apikeys_processing:get_api_key(ApiKeyId) of
            {ok, ApiKey} ->
                akm_handler_utils:reply_ok(200, ApiKey);
            {error, not_found} ->
                akm_handler_utils:reply_error(404)
        end
    end,
    {ok, #{authorize => Authorize, process => Process}}
.
