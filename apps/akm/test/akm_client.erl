-module(akm_client).

-include_lib("opentelemetry_api/include/otel_tracer.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").

%% API
-export([
    issue_key/4,
    get_key/4,
    list_keys/4,
    list_keys/3,
    request_revoke_key/4,
    revoke_key/3
]).

-spec issue_key(inet:hostname() | inet:ip_address(), inet:port_number(), binary(), map()) -> any().
issue_key(Host, Port, PartyId, ApiKey) ->
    perform_request(Host, Port, <<"issue_key">>, fun(ConnPid, Headers) ->
        Path = <<"/apikeys/v2/orgs/", PartyId/binary, "/api-keys">>,
        Body = jsx:encode(ApiKey),
        post(ConnPid, Path, Headers, Body)
    end).

-spec get_key(inet:hostname() | inet:ip_address(), inet:port_number(), binary(), binary()) -> any().
get_key(Host, Port, PartyId, ApiKeyId) ->
    perform_request(Host, Port, <<"get_key">>, fun(ConnPid, Headers) ->
        Path = <<"/apikeys/v2/orgs/", PartyId/binary, "/api-keys/", ApiKeyId/binary>>,
        get(ConnPid, Path, Headers)
    end).

-spec list_keys(inet:hostname() | inet:ip_address(), inet:port_number(), binary()) -> any().
list_keys(Host, Port, PartyId) ->
    list_keys(Host, Port, PartyId, [{<<"limit">>, <<"1000">>}]).

-spec list_keys(inet:hostname() | inet:ip_address(), inet:port_number(), binary(), list()) -> any().
list_keys(Host, Port, PartyId, QsList) ->
    perform_request(Host, Port, <<"list_keys">>, fun(ConnPid, Headers) ->
        Path = <<"/apikeys/v2/orgs/", PartyId/binary, "/api-keys">>,
        PathWithQuery = maybe_query(Path, QsList),
        get(ConnPid, PathWithQuery, Headers)
    end).

-spec request_revoke_key(inet:hostname() | inet:ip_address(), inet:port_number(), binary(), binary()) -> any().
request_revoke_key(Host, Port, PartyId, ApiKeyId) ->
    perform_request(Host, Port, <<"request_revoke">>, fun(ConnPid, Headers) ->
        Path = <<"/apikeys/v2/orgs/", PartyId/binary, "/api-keys/", ApiKeyId/binary, "/status">>,
        Body = jsx:encode(#{<<"status">> => <<"revoked">>}),
        put(ConnPid, Path, Headers, Body)
    end).

-spec revoke_key(inet:hostname() | inet:ip_address(), inet:port_number(), binary()) -> any().
revoke_key(Host, Port, PathWithQuery) ->
    perform_request(Host, Port, <<"revoke_key">>, fun(ConnPid, Headers) ->
        get(ConnPid, PathWithQuery, Headers)
    end).

% Internal functions

perform_request(Host, Port, RequestID, F) ->
    SpanName = iolist_to_binary(["client ", RequestID]),
    ?with_span(SpanName, #{kind => ?SPAN_KIND_CLIENT}, fun(_SpanCtx) ->
        Headers = prepare_headers(RequestID),
        ConnPid = connect(Host, Port),
        Answer = F(ConnPid, Headers),
        disconnect(ConnPid),
        parse(Answer)
    end).

prepare_headers(RequestID) ->
    otel_propagator_text_map:inject([
        {<<"X-Request-ID">>, RequestID},
        {<<"content-type">>, <<"application/json; charset=utf-8">>},
        {<<"Authorization">>, <<"Bearer sffsdfsfsdfsdfs">>}
    ]).

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

put(ConnPid, Path, Headers, Body) ->
    StreamRef = gun:put(ConnPid, Path, Headers, Body),
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
