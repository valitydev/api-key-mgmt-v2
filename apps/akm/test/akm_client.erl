-module(akm_client).

%% API
-export([
    issue_key/4,
    get_key/4,
    list_keys/4,
    list_keys/3
]).

-spec issue_key(inet:hostname() | inet:ip_address(), inet:port_number(), binary(), map()) -> any().
issue_key(Host, Port, PartyId, ApiKey) ->
    Path = <<"/apikeys/v2/orgs/", PartyId/binary, "/api-keys">>,
    Body = jsx:encode(ApiKey),
    Headers = [
        {<<"X-Request-ID">>, <<"issue_key">>},
        {<<"content-type">>, <<"application/json; charset=utf-8">>},
        {<<"Authorization">>, <<"Bearer sffsdfsfsdfsdfs">>}
    ],
    ConnPid = connect(Host, Port),
    Answer = post(ConnPid, Path, Headers, Body),
    disconnect(ConnPid),
    parse(Answer).

-spec get_key(inet:hostname() | inet:ip_address(), inet:port_number(), binary(), binary()) -> any().
get_key(Host, Port, PartyId, ApiKeyId) ->
    Path = <<"/apikeys/v2/orgs/", PartyId/binary, "/api-keys/", ApiKeyId/binary>>,
    Headers = [
        {<<"X-Request-ID">>, <<"get_key">>},
        {<<"content-type">>, <<"application/json; charset=utf-8">>},
        {<<"Authorization">>, <<"Bearer sffsdfsfsdfsdfs">>}
    ],
    ConnPid = connect(Host, Port),
    Answer = get(ConnPid, Path, Headers),
    disconnect(ConnPid),
    parse(Answer).

-spec list_keys(inet:hostname() | inet:ip_address(), inet:port_number(), binary()) -> any().
list_keys(Host, Port, PartyId) ->
    list_keys(Host, Port, PartyId, [{<<"limit">>, <<"1000">>}]).

-spec list_keys(inet:hostname() | inet:ip_address(), inet:port_number(), binary(), list()) -> any().
list_keys(Host, Port, PartyId, QsList) ->
    Path = <<"/apikeys/v2/orgs/", PartyId/binary, "/api-keys">>,
    Headers = [
        {<<"X-Request-ID">>, <<"list_keys">>},
        {<<"content-type">>, <<"application/json; charset=utf-8">>},
        {<<"Authorization">>, <<"Bearer sffsdfsfsdfsdfs">>}
    ],
    PathWithQuery = maybe_query(Path, QsList),
    ConnPid = connect(Host, Port),
    Answer = get(ConnPid, PathWithQuery, Headers),
    disconnect(ConnPid),
    parse(Answer).

% Internal functions

-spec connect(inet:hostname() | inet:ip_address(), inet:port_number()) -> any().
connect(Host, Port) ->
    connect(Host, Port, #{}).

-spec connect(inet:hostname() | inet:ip_address(), inet:port_number(), map()) -> any().
connect(Host, Port, Opts) ->
    {ok, ConnPid} = gun:open(Host, Port, Opts),
    {ok, _} = gun:await_up(ConnPid),
    ConnPid.

disconnect(ConnPid) ->
    gun:close(ConnPid).

get(ConnPid, Path, Headers) ->
    StreamRef = gun:get(ConnPid, Path, Headers),
    get_response(ConnPid, StreamRef).

post(ConnPid, Path, Headers, Body) ->
    StreamRef = gun:post(ConnPid, Path, Headers, Body),
    get_response(ConnPid, StreamRef).

get_response(ConnPid, StreamRef) ->
    case gun:await(ConnPid, StreamRef) of
        {response, fin, Status, Headers} ->
            {Status, Headers, <<>>};
        {response, nofin, Status, Headers} ->
            {ok, Body} = gun:await_body(ConnPid, StreamRef),
            {Status, Headers, Body}
    end.

maybe_query(Path, []) ->
    Path;
maybe_query(Path, QsList) ->
    QS = uri_string:compose_query(QsList),
    <<Path/binary, "?", QS/binary>>.

parse({200, _, Body}) -> jsx:decode(Body, [return_maps]);
parse({404, _, _}) -> not_found;
parse(Other) -> Other.
