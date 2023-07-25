%% @doc Top level supervisor.
%% @end

-module(akm_sup).

-behaviour(supervisor).

-include("akm.hrl").

-define(TEMPLATE_FILE, "request_revoke.dtl").
-define(TEMPLATE_DIR, "/opt/api-key-mgmt-v2/templates").
-define(SECRET_KEY, <<"api-key-mgmt-v2/mail_pass">>).

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
    ok = dbinit(),
    ok = start_vault_client(),
    ok = set_mail_pass(),
    {ok, _} = compile_template(),
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
            akm_handler => {akm_handler, #{}}
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

get_env_var(Name) ->
    case os:getenv(Name) of
        false -> throw({os_env_required, Name});
        V -> V
    end.

dbinit() ->
    WorkDir = get_env_var("WORK_DIR"),
    EnvPath = WorkDir ++ "/.env",
    MigrationsPath = WorkDir ++ "/migrations",
    Cmd = "run",
    case akm_db_migration:process(["-e", EnvPath, "-d", MigrationsPath, Cmd]) of
        ok -> ok;
        {error, Reason} -> throw({migrations_error, Reason})
    end.

compile_template() ->
    TemplateFile = template_file(),
    File =
        case filelib:is_file(TemplateFile) of
            true -> TemplateFile;
            false -> default_template_file()
        end,
    AkmEbinDir = code:lib_dir(akm, ebin),
    erlydtl:compile({file, File}, ?RENDER_MODULE, [{out_dir, AkmEbinDir}]).

default_template_file() ->
    AkmPrivDir = code:priv_dir(akm),
    filename:join([AkmPrivDir, "mails", ?TEMPLATE_FILE]).

template_file() ->
    filename:join([?TEMPLATE_DIR, ?TEMPLATE_FILE]).

start_vault_client() ->
    {ok, TokenPath} = application:get_env(akm, vault_token_path),
    {ok, Role} = application:get_env(akm, vault_role),
    {ok, Token} = read_maybe_linked_file(TokenPath),
    AuthMethod = {kubernetes, unicode:characters_to_binary(Role), Token},
    canal:auth(AuthMethod).

read_maybe_linked_file(MaybeLinkName) ->
    case file:read_link(MaybeLinkName) of
        {error, enoent} = Result ->
            Result;
        {error, einval} ->
            file:read_file(MaybeLinkName);
        {ok, Filename} ->
            file:read_file(maybe_expand_relative(MaybeLinkName, Filename))
    end.

maybe_expand_relative(BaseFilename, Filename) ->
    filename:absname_join(filename:dirname(BaseFilename), Filename).

set_mail_pass() ->
    {ok, MailerOpts} = application:get_env(akm, mailer),
    {ok, #{<<"value">> := MailPass}} = canal:read(?SECRET_KEY),
    application:set_env(akm, mailer, MailerOpts#{password => unicode:characters_to_list(MailPass)}).
