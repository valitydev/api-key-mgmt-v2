-module(akm_apikeys_processing).

-include_lib("bouncer_proto/include/bouncer_ctx_v1_thrift.hrl").
-include_lib("bouncer_proto/include/bouncer_ctx_thrift.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-export([issue_api_key/3]).
-export([get_api_key/1]).
-export([list_api_keys/4]).
-export([request_revoke/4]).
-export([revoke/3]).

-type list_keys_response() :: #{
    results => [map()],
    continuationToken := binary()
}.

-type woody_context() :: woody_context:ctx().

-spec issue_api_key(_, _, _) -> _.
issue_api_key(PartyID, #{<<"name">> := Name} = ApiKey0, WoodyContext) ->
    Metadata0 = maps:get(<<"metadata">>, ApiKey0, #{}),
    %%  REWORK ненормальный ID, переработать
    ID = akm_id:generate_snowflake_id(),
    ContextV1Fragment = bouncer_context_helpers:make_auth_fragment(#{
        method => <<"ApiKeyToken">>,
        scope => [#{party => #{id => PartyID}}],
        token => #{id => ID}
    }),
    {encoded_fragment, ContextFragment} = bouncer_client:bake_context_fragment(ContextV1Fragment),
    Status = "active",
    Metadata = akm_auth:put_party_to_metadata(PartyID, Metadata0),
    Client = token_keeper_client:offline_authority(get_authority_id(), WoodyContext),
    case token_keeper_authority_offline:create(ID, ContextFragment, Metadata, Client) of
        {ok, #{token := Token}} ->
            {ok, _, Columns, Rows} = epgsql_pool:query(
                main_pool,
                "INSERT INTO apikeys (id, name, party_id, status, pending_status, metadata)"
                "VALUES ($1, $2, $3, $4, $5, $6) RETURNING id, name, status, metadata, created_at",
                [ID, Name, PartyID, Status, Status, jsx:encode(Metadata)]
            ),
            [ApiKey | _] = to_marshalled_maps(Columns, Rows),
            Resp = #{
                <<"accessToken">> => Token,
                <<"apiKey">> => ApiKey
            },
            {ok, Resp};
        {error, {auth_data, already_exists}} ->
            {error, already_exists}
    end.

-spec get_api_key(binary()) -> {ok, map()} | {error, not_found}.
get_api_key(ApiKeyId) ->
    Result = epgsql_pool:query(
        main_pool,
        "SELECT id, name, status, metadata, created_at FROM apikeys WHERE id = $1",
        [ApiKeyId]
    ),
    case Result of
        {ok, _Columns, []} ->
            {error, not_found};
        {ok, Columns, Rows} ->
            [ApiKey | _] = to_marshalled_maps(Columns, Rows),
            {ok, ApiKey}
    end.

-spec list_api_keys(binary(), binary(), non_neg_integer(), non_neg_integer()) -> {ok, list_keys_response()}.
list_api_keys(PartyID, Status, Limit, Offset) ->
    {ok, Columns, Rows} = get_keys(PartyID, Status, Limit, Offset),
    case erlang:length(Rows) < Limit of
        true ->
            % last piece of data
            {ok, #{results => to_marshalled_maps(Columns, Rows)}};
        false ->
            {ok, #{
                results => to_marshalled_maps(Columns, Rows),
                continuationToken => erlang:integer_to_binary(Offset + Limit)
            }}
    end.

-spec request_revoke(binary(), binary(), binary(), binary()) ->
    {ok, revoke_email_sent} | {error, not_found}.
request_revoke(Email, PartyID, ApiKeyId, Status) ->
    case get_full_api_key(ApiKeyId) of
        {error, not_found} ->
            {error, not_found};
        {ok, _ApiKey} ->
            Token = akm_id:generate_snowflake_id(),
            try
                epgsql_pool:transaction(
                    main_pool,
                    fun(Worker) ->
                        ok = akm_mailer:send_revoke_mail(Email, PartyID, ApiKeyId, Token),
                        epgsql_pool:query(
                            Worker,
                            "UPDATE apikeys SET pending_status = $1, revoke_token = $2 "
                            "WHERE id = $3",
                            [Status, Token, ApiKeyId]
                        )
                    end
                )
            of
                {ok, 1} ->
                    {ok, revoke_email_sent}
            catch
                Ex:Er ->
                    logger:error("Failed to send email with ~p:~p", [Ex, Er]),
                    error(failed_to_send_email)
            end
    end.

-spec revoke(binary(), binary(), woody_context()) -> ok | {error, not_found}.
revoke(ApiKeyId, RevokeToken, WoodyContext) ->
    case get_full_api_key(ApiKeyId) of
        {ok, #{
            <<"pending_status">> := PendingStatus,
            <<"revoke_token">> := RevokeToken
        }} ->
            Client = token_keeper_client:offline_authority(get_authority_id(), WoodyContext),
            try
                epgsql_pool:transaction(
                    main_pool,
                    fun(Worker) ->
                        {ok, _} = token_keeper_authority_offline:revoke(ApiKeyId, Client),
                        epgsql_pool:query(
                            Worker,
                            "UPDATE apikeys SET status = $1, revoke_token = null WHERE id = $2",
                            [PendingStatus, ApiKeyId]
                        )
                    end
                )
            of
                {ok, 1} -> ok
            catch
                Ex:Er ->
                    logger:error("Can`t revoke ApiKey ~p with error: ~p:~p", [ApiKeyId, Ex, Er]),
                    {error, not_found}
            end;
        _ ->
            {error, not_found}
    end.

%% Internal functions

get_authority_id() ->
    application:get_env(akm, authority_id, undefined).

get_full_api_key(ApiKeyId) ->
    Result = epgsql_pool:query(
        main_pool,
        "SELECT * FROM apikeys WHERE id = $1",
        [ApiKeyId]
    ),
    case Result of
        {ok, _Columns, []} ->
            {error, not_found};
        {ok, Columns, Rows} ->
            [ApiKey | _] = to_maps(Columns, Rows),
            {ok, ApiKey}
    end.

get_keys(PartyID, undefined, Limit, Offset) ->
    epgsql_pool:query(
        main_pool,
        "SELECT id, name, status, metadata, created_at FROM apikeys where party_id = $1 "
        "ORDER BY created_at DESC LIMIT $2 OFFSET $3",
        [PartyID, Limit, Offset]
    );
get_keys(PartyID, Status, Limit, Offset) ->
    epgsql_pool:query(
        main_pool,
        "SELECT id, name, status, metadata, created_at FROM apikeys where party_id = $1 AND status = $2 "
        "ORDER BY created_at DESC LIMIT $3 OFFSET $4",
        [PartyID, Status, Limit, Offset]
    ).

%% Encode/Decode

to_marshalled_maps(Columns, Rows) ->
    to_maps(Columns, Rows, fun marshall_api_key/1).

to_maps(Columns, Rows) ->
    to_maps(Columns, Rows, fun(V) -> V end).

to_maps(Columns, Rows, TransformRowFun) ->
    ColNumbers = erlang:length(Columns),
    Seq = lists:seq(1, ColNumbers),
    lists:map(
        fun(Row) ->
            Data = lists:foldl(
                fun(Pos, Acc) ->
                    #column{name = Field, type = Type} = lists:nth(Pos, Columns),
                    Acc#{Field => convert(Type, erlang:element(Pos, Row))}
                end,
                #{},
                Seq
            ),
            TransformRowFun(Data)
        end,
        Rows
    ).

%% for reference https://github.com/epgsql/epgsql#data-representation
convert(timestamp, Value) ->
    datetime_to_binary(Value);
convert(timestamptz, Value) ->
    datetime_to_binary(Value);
convert(_Type, Value) ->
    Value.

datetime_to_binary({Date, {Hour, Minute, Second}}) when is_float(Second) ->
    datetime_to_binary({Date, {Hour, Minute, trunc(Second)}});
datetime_to_binary(DateTime) ->
    UnixTime = genlib_time:daytime_to_unixtime(DateTime),
    genlib_rfc3339:format(UnixTime, second).

decode_json(null) -> #{};
decode_json(Value) -> jsx:decode(Value, [return_maps]).

%% Marshalling

marshall_api_key(#{
    <<"id">> := ID,
    <<"created_at">> := DateTime,
    <<"name">> := Name,
    <<"status">> := Status,
    <<"metadata">> := Metadata
}) ->
    #{
        <<"id">> => ID,
        <<"createdAt">> => DateTime,
        <<"name">> => Name,
        <<"status">> => Status,
        <<"metadata">> => decode_json(Metadata)
    }.
