-module(akm_client).

%% API
-export([
    issue_key/4,
    get_key/4
]).

-spec issue_key(_, _, _, _) -> _.
issue_key(Host, Port, PartyId, ApiKey) ->
    Path = <<"/apikeys/v2/orgs/", PartyId/binary, "/api-keys">>,
    Body = jsx:encode(ApiKey),
    Headers = [
        {<<"X-Request-ID">>, <<"issue_key_success_test">>},
        {<<"content-type">>, <<"application/json; charset=utf-8">>},
        {<<"Authorization">>, <<"Bearer sffsdfsfsdfsdfs">>}
    ],
    ConnPid = connect(Host, Port),
    Answer = post(ConnPid, Path, Headers, Body),
    disconnect(ConnPid),
    parse(Answer).

-spec get_key(_, _, _, _) -> _.
get_key(Host, Port, PartyId, ApiKeyId) ->
    Path = <<"/apikeys/v2/orgs/", PartyId/binary, "/api-keys/", ApiKeyId/binary>>,
    Headers = [
        {<<"X-Request-ID">>, <<"get_key_success_test">>},
        {<<"content-type">>, <<"application/json; charset=utf-8">>},
        {<<"Authorization">>, <<"Bearer sffsdfsfsdfsdfs">>}
    ],
    ConnPid = connect(Host, Port),
    Answer = get(ConnPid, Path, Headers),
    disconnect(ConnPid),
    parse(Answer).

% Internal functions

connect(Host, Port) ->
    connect(Host, Port, #{}).

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

parse({200, _, Body}) -> jsx:decode(Body, [return_maps]);
parse(Other) -> Other.
