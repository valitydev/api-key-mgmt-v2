-module(akm_apikeys_processing).

-include_lib("bouncer_proto/include/bouncer_ctx_v1_thrift.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-export([issue_api_key/3]).
-export([get_api_key/1]).

-spec issue_api_key(_, _, _) -> _.
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
            {ok, _Columns, [{CreatedAt}]} = epgsql_pool:query(
                main_query,
                "INSERT INTO apikeys (id, name, party_id, status, metadata)"
                "VALUES ($1, $2, $3, $4, $5) RETURNING created_at",
                [ID, Name, PartyID, Status, Metadata]
            ),
            ApiKey = #{
                id => ID,
                name => Name,
                created_at => CreatedAt,
                status => Status,
                metadata => Metadata
            },
            Resp = #{
                <<"AccessToken">> => marshall_access_token(Token),
                <<"ApiKey">> => marshall_api_key(ApiKey)
            },
            {ok, Resp};
        {error, {auth_data, already_exists}} ->
            {error, already_exists}
    end.

-spec get_api_key(binary()) -> {ok, map()} | {error, not_found}.
get_api_key(ApiKeyId) ->
    Result = epgsql_pool:query(
        main_pool,
        "SELECT id, name, status, metadata, created_at FROM apikeys where id = $1",
        [ApiKeyId]
    ),
    case Result of
        {ok, _Columns, []} ->
            {error, not_found};
        {ok, Columns, Rows} ->
            [ApiKey | _ ] = to_maps(Columns, Rows),
            {ok, ApiKey}
    end.

get_authority_id() ->
    application:get_env(akm, authority_id).

%% Marshalling

marshall_api_key(#{
        id := ID,
        name := Name,
        created_at := CreatedAt,
        status := Status,
        metadata := Metadata
    }) ->
    #{
        <<"id">> => ID,
        <<"createdAt">> => CreatedAt,
        <<"name">> => Name,
        <<"status">> => Status,
        <<"metadata">> => Metadata
    }.

marshall_access_token(Token) ->
    #{
        <<"accessToken">> => Token
    }.

to_maps(Columns, Rows) ->
    ColNumbers = erlang:length(Columns),
    ColumnsWithConvertedNames = lists:map(
        fun(#column{name = Name} = Col) ->
            Col#column{name = snake_to_camel(Name)}
        end,
        Columns
    ),
    Seq = lists:seq(1, ColNumbers),
    lists:map(fun(Row) ->
        lists:foldl(fun(Pos, Acc) ->
            #column{name = Field, type = Type} = lists:nth(Pos, ColumnsWithConvertedNames),
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

%% create_at -> createAt
snake_to_camel(Data) ->
    [H | Chunks] = binary:split(Data, <<"_">>, [global]),
    ConvertedTail = lists:map(fun string:titlecase/1, Chunks),
    lists:foldl(fun(Bin, Acc) -> <<Acc/binary, Bin/binary>> end, H, ConvertedTail).