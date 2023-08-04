-module(akm_handler).

-behaviour(swag_server_apikeys_logic_handler).

%% swag_server_apikeys_logic_handler callbacks
-export([map_error/2]).
-export([authorize_api_key/4]).
-export([handle_request/4]).

-type opts() :: swag_server_apikeys:handler_opts(_).

%% API

%% @WARNING Must be refactored in case of different classes of users using this API
%% See CAPI capi_handler
%% https://github.com/valitydev/capi-v2/blob/2de9367561a511f0dc1448881201de48e9004c54/apps/capi/src/capi_handler.erl#L62
-define(REALM, <<"external">>).

-spec map_error(atom(), swag_server_apikeys_validation:error()) -> swag_server_apikeys:error_reason().
map_error(validation_error, Error) ->
    Type = map_error_type(maps:get(type, Error)),
    Name = genlib:to_binary(maps:get(param_name, Error)),
    Message =
        case maps:get(description, Error, undefined) of
            undefined ->
                <<"Request parameter: ", Name/binary, ", error type: ", Type/binary>>;
            Description ->
                DescriptionBin = genlib:to_binary(Description),
                <<"Request parameter: ", Name/binary, ", error type: ", Type/binary, ", description: ",
                    DescriptionBin/binary>>
        end,
    jsx:encode(#{
        <<"errorType">> => Type,
        <<"name">> => Name,
        <<"description">> => Message
    }).

-spec map_error_type(swag_server_apikeys_validation:error_type()) -> binary().
map_error_type(no_match) -> <<"NoMatch">>;
map_error_type(not_found) -> <<"NotFound">>;
map_error_type(not_in_range) -> <<"NotInRange">>;
map_error_type(wrong_length) -> <<"WrongLength">>;
map_error_type(wrong_size) -> <<"WrongSize">>;
map_error_type(schema_violated) -> <<"SchemaViolated">>;
map_error_type(wrong_type) -> <<"WrongType">>;
map_error_type(wrong_format) -> <<"WrongFormat">>;
map_error_type(wrong_body) -> <<"WrongBody">>.

-spec authorize_api_key(
    swag_server_apikeys:operation_id(),
    swag_server_apikeys:api_key(),
    swag_server_apikeys:request_context(),
    opts()
) ->
    Result :: false | {true, akm_auth:preauth_context()}.
authorize_api_key(OperationID, ApiKey, _Context, _HandlerOpts) ->
    %% Since we require the request id field to create a woody context for our trip to token_keeper
    %% it seems it is no longer possible to perform any authorization in this method.
    %% To gain this ability back be would need to rewrite the swagger generator to perform its
    %% request validation checks before this stage.
    %% But since a decent chunk of authorization logic is already defined in the handler function
    %% it is probably easier to move it there in its entirety.
    ok = scoper:add_scope('swag.server', #{api => apikeymgmt, operation_id => OperationID}),
    case akm_auth:preauthorize_api_key(ApiKey) of
        {ok, Context} ->
            {true, Context};
        {error, Error} ->
            _ = logger:info("API Key preauthorization failed for ~p due to ~p", [OperationID, Error]),
            false
    end.

-spec handle_request(
    swag_server_apikeys:operation_id(),
    akm_apikeys_handler:request_data(),
    swag_server_apikeys:request_context(),
    opts()
) ->
    akm_apikeys_handler:request_result().
handle_request(OperationID, Req, SwagContext, Opts) ->
    #{'X-Request-Deadline' := Header} = Req,
    case akm_utils:parse_deadline(Header) of
        {ok, Deadline} ->
            WoodyContext = attach_deadline(Deadline, create_woody_context(Req)),
            process_request(OperationID, Req, SwagContext, Opts, WoodyContext);
        _ ->
            akm_handler_utils:reply_ok(400, #{
                <<"errorType">> => <<"SchemaViolated">>,
                <<"name">> => <<"X-Request-Deadline">>,
                <<"description">> => <<"Invalid data in X-Request-Deadline header">>
            })
    end.

process_request(OperationID, Req, SwagContext0, Opts, WoodyContext0) ->
    _ = logger:info("Processing request ~p", [OperationID]),
    try
        SwagContext = do_authorize_api_key(OperationID, SwagContext0, WoodyContext0),
        WoodyContext = put_user_identity(WoodyContext0, get_auth_context(SwagContext)),
        Context = create_handler_context(OperationID, SwagContext, WoodyContext),
        ok = set_context_meta(Context),
        {ok, RequestState} = akm_apikeys_handler:prepare(OperationID, Req, Context, Opts),
        #{authorize := Authorize, process := Process} = RequestState,
        {ok, Resolution} = Authorize(),
        case Resolution of
            allowed ->
                ok = logger:debug("Operation ~p authorized", [OperationID]),
                Process();
            forbidden ->
                _ = logger:info("Authorization failed"),
                akm_handler_utils:reply_ok(401)
        end
    catch
        throw:{token_auth_failed, Reason} ->
            _ = logger:info("API Key authorization failed for ~p due to ~p", [OperationID, Reason]),
            akm_handler_utils:reply_ok(401);
        error:{woody_error, {Source, Class, Details}} ->
            process_woody_error(Source, Class, Details)
    end.

-spec create_woody_context(akm_apikeys_handler:request_data()) -> woody_context:ctx().
create_woody_context(RequestData) ->
    %% use dynamic request_id if not presented
    RequestID = maps:get('X-Request-ID', RequestData, new_request_id()),
    RpcID = #{trace_id := TraceID} = woody_context:new_rpc_id(genlib:to_binary(RequestID)),
    ok = scoper:add_meta(#{request_id => RequestID, trace_id => TraceID}),
    woody_context:new(RpcID, undefined, akm_woody_client:get_service_deadline(akm)).

put_user_identity(WoodyContext, AuthContext) ->
    woody_user_identity:put(collect_user_identity(AuthContext), WoodyContext).

get_auth_context(#{auth_context := AuthContext}) ->
    AuthContext.

collect_user_identity(AuthContext) ->
    genlib_map:compact(#{
        id => akm_auth:get_subject_id(AuthContext),
        %%TODO: Store user realm in authdata meta and extract it here
        realm => ?REALM,
        email => akm_auth:get_user_email(AuthContext)
    }).

-spec set_context_meta(akm_handler_utils:handler_context()) -> ok.
set_context_meta(Context) ->
    AuthContext = akm_handler_utils:get_auth_context(Context),
    Meta = #{
        metadata => #{
            'user-identity' => collect_user_identity(AuthContext)
        }
    },
    scoper:add_meta(Meta).

attach_deadline(undefined, Context) ->
    Context;
attach_deadline(Deadline, Context) ->
    woody_context:set_deadline(Deadline, Context).

do_authorize_api_key('RevokeApiKey', SwagContext, _WoodyContext) ->
    SwagContext;
do_authorize_api_key(_OperationID, SwagContext = #{auth_context := PreAuthContext}, WoodyContext) ->
    case akm_auth:authorize_api_key(PreAuthContext, make_token_context(SwagContext), WoodyContext) of
        {ok, AuthContext} ->
            SwagContext#{auth_context => AuthContext};
        {error, Error} ->
            throw({token_auth_failed, Error})
    end.

make_token_context(#{cowboy_req := CowboyReq}) ->
    case cowboy_req:header(<<"origin">>, CowboyReq) of
        Origin when is_binary(Origin) ->
            #{request_origin => Origin};
        undefined ->
            #{}
    end.

-spec create_handler_context(
    swag_server_apikeys:operation_id(), swag_server_apikeys:request_context(), woody_context:ctx()
) -> akm_handler_utils:handler_context().
create_handler_context(OpID, SwagContext, WoodyContext) ->
    #{
        operation_id => OpID,
        woody_context => WoodyContext,
        swagger_context => SwagContext,
        swag_server_get_schema_fun => fun swag_server_apikeys_schema:get/0,
        swag_server_get_operation_fun => fun(OperationID) -> swag_server_apikeys_router:get_operation(OperationID) end
    }.

process_woody_error(_Source, result_unexpected, _Details) ->
    akm_handler_utils:reply_error(500);
process_woody_error(_Source, resource_unavailable, _Details) ->
    % Return an 504 since it is unknown if state of the system has been altered
    % @TODO Implement some sort of tagging for operations that mutate the state,
    % so we can still return 503s for those that don't
    akm_handler_utils:reply_error(504);
process_woody_error(_Source, result_unknown, _Details) ->
    akm_handler_utils:reply_error(504).

new_request_id() ->
    base64:encode(crypto:strong_rand_bytes(24)).
