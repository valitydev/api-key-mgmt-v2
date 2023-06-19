-module(akm_apikeys_processing).

-include_lib("bouncer_proto/include/bouncer_ctx_v1_thrift.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-export([issue_api_key/3]).
-export([get_api_key/1]).

issue_api_key(PartyID, ApiKey, WoodyContext) ->
    #{
        <<"name">> := Name,
        <<"metadata">> := Metadata
    } = ApiKey,
%%  REWORK ненормальный ID, переработать
    ID = akm_id:generate_snowflake_id(),
    ContextFragment = bouncer_context_helpers:make_auth_fragment(#{
        method => <<"IssueApiKey">>,
        scope => [#{party => #{id => PartyID}}],
        token => #{id => ID}
    }),
    Status = "active",
    Metadata = #{
        <<"party.id">> => PartyID
    },
    Client = token_keeper_client:offline_authority(get_authority_id(), WoodyContext),
    case token_keeper_authority_offline:create(ID, ContextFragment, Metadata, Client) of
        {ok, #{token := Token}} ->
            {ok, 1} = epgsql_pool:query(
                main_query,
                "INSERT INTO apikeys (id, name, party_id, status, metadata) VALUES ($1, $2, $3, $4, $5)",
                [ID, Name, PartyID, Status, Metadata]
            ),
            #{
                id => ID,
                name => Name,
                party_id => PartyID,
                status => Status,
                metadata => Metadata,
                token => Token
            };
        {error, {auth_data, already_exists}} ->
            {error, already_exists}
    end.

get_api_key(ApiKeyId) ->
    Result = epgsql_pool:query(
        main_pool,
        "SELECT id, name, party_id, status, metadata FROM apikeys where id = $1",
        [ApiKeyId]
    ),
    case Result of
        {ok, _Columns, []} ->
            {error, not_found};
        {ok, Columns, Rows} ->
            [ApiKey | _ ] = to_maps(Columns, Rows),
            {ok, ApiKey}
    end.

%% Internal functions

get_authority_id() ->
    application:get_env(akm, authority_id).

to_maps(Columns, Rows) ->
    ColNumbers = erlang:length(Columns),
    Seq = lists:seq(1, ColNumbers),
    lists:map(fun(Row) ->
        lists:foldl(fun(Pos, Acc) ->
            #column{name = Field, type = Type} = lists:nth(Pos, Columns),
            add_field(Field, convert(Type, erlang:element(Pos, Row)), Acc)
        end, #{}, Seq)
    end, Rows).

add_field(_FieldName, null, Acc) -> Acc;
add_field(FieldName, Value, Acc) -> Acc#{FieldName => Value}.

%% for reference https://github.com/epgsql/epgsql#data-representation
convert(timestamp, Value) ->
    datetime_to_binary(Value);
convert(_Type, Value) -> Value.

datetime_to_binary({Date, {Hour, Minute, Second}}) when is_float(Second) ->
    datetime_to_binary({Date, {Hour, Minute, trunc(Second)}});
datetime_to_binary(DateTime) ->
    UnixTime = genlib_time:daytime_to_unixtime(DateTime),
    genlib_rfc3339:format(UnixTime, second).
