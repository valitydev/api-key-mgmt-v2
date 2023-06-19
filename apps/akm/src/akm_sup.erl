%% @doc Top level supervisor.
%% @end

-module(akm_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%

-spec start_link() -> {ok, pid()} | {error, {already_started, pid()}}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {LogicHandlers, LogicHandlerSpecs} = get_logic_handler_info(),
    HealthCheck = enable_health_logging(genlib_app:env(akm, health_check, #{})),
    AdditionalRoutes = [{'_', [erl_health_handle:get_route(HealthCheck), get_prometheus_route()]}],
    SwaggerHandlerOpts = genlib_app:env(akm, swagger_handler_opts, #{}),
    SwaggerSpec = akm_swagger_server:child_spec(AdditionalRoutes, LogicHandlers, SwaggerHandlerOpts),
    ok = start_epgsql_pooler(),
    {ok, {
        {one_for_all, 0, 1},
            LogicHandlerSpecs ++ [SwaggerSpec]
    }}.

-spec get_logic_handler_info() -> {akm_swagger_server:logic_handlers(), [supervisor:child_spec()]}.
get_logic_handler_info() ->
    {
        #{
            keys => {akm_handler, #{}}
        },
        []
    }.

-spec enable_health_logging(erl_health:check()) -> erl_health:check().
enable_health_logging(Check) ->
    EvHandler = {erl_health_event_handler, []},
    maps:map(fun(_, V = {_, _, _}) -> #{runner => V, event_handler => EvHandler} end, Check).

start_epgsql_pooler() ->
    Params = genlib_app:env(akm, epsql_connection, #{}),
    ok = epgsql_pool:validate_connection_params(Params),
    {ok, _} = epgsql_pool:start(main_pool, 10, 20, Params),
    ok.

-spec get_prometheus_route() -> {iodata(), module(), _Opts :: any()}.
get_prometheus_route() ->
    {"/metrics/[:registry]", prometheus_cowboy2_handler, []}.
