%% @doc Top level supervisor.
%% @end

-module(akm_sup).

-behaviour(supervisor).

-include("akm.hrl").

-define(TEMPLATE_FILE, "request_revoke.dtl").
-define(TEMPLATE_DIR, "/opt/api-key-mgmt-v2/templates").
-define(VAULT_TOKEN_PATH, "/var/run/secrets/kubernetes.io/serviceaccount/token").
-define(VAULT_ROLE, <<"api-key-mgmt-v2">>).
-define(VAULT_KEY_PG_CREDS, <<"api-key-mgmt-v2/pg_creds">>).

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
    ok = maybe_set_secrets(),
    ok = dbinit(),
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
    _ = set_database_url(),
    MigrationsPath = WorkDir ++ "/migrations",
    Cmd = "run",
    case akm_db_migration:process(["-d", MigrationsPath, Cmd]) of
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

set_database_url() ->
    {ok, #{
        host := PgHost,
        port := PgPort,
        username := PgUser,
        password := PgPassword,
        database := DbName
    }} = application:get_env(akm, epsql_connection),
    %% DATABASE_URL=postgresql://postgres:postgres@db/apikeymgmtv2
    PgPortStr = erlang:integer_to_list(PgPort),
    Value =
        "postgresql://" ++ PgUser ++ ":" ++ PgPassword ++ "@" ++ PgHost ++ ":" ++ PgPortStr ++ "/" ++ DbName,
    true = os:putenv("DATABASE_URL", Value).

maybe_set_secrets() ->
    TokenPath = application:get_env(akm, vault_token_path, ?VAULT_TOKEN_PATH),
    try vault_client_auth(TokenPath) of
        ok ->
            Key = application:get_env(akm, vault_key_pg_creds, ?VAULT_KEY_PG_CREDS),
            set_secrets(canal:read(Key));
        Error ->
            logger:error("can`t auth vault client with error: ~p", [Error]),
            skip
    catch
        _:_ ->
            logger:error("catch exception when auth vault client"),
            skip
    end,
    ok.

set_secrets(
    {
        ok, #{
            <<"pg_creds">> := #{
                <<"pg_user">> := PgUser,
                <<"pg_password">> := PgPassword
            }
        }
    }
) ->
    logger:info("postgres credentials successfuly read from vault (as json)"),
    {ok, ConnOpts} = application:get_env(akm, epsql_connection),
    application:set_env(
        akm,
        epsql_connection,
        ConnOpts#{
            username => unicode:characters_to_list(PgUser),
            password => unicode:characters_to_list(PgPassword)
        }
    ),
    ok;
set_secrets({ok, #{<<"pg_creds">> := PgCreds}}) ->
    logger:info("postgres credentials successfuly read from vault (as string)"),
    set_secrets({ok, #{<<"pg_creds">> => jsx:decode(PgCreds, [return_maps])}});
set_secrets(Error) ->
    logger:error("can`t read postgres credentials from vault with error: ~p", [Error]),
    skip.

vault_client_auth(TokenPath) ->
    case read_maybe_linked_file(TokenPath) of
        {ok, Token} ->
            Role = application:get_env(akm, vault_role, ?VAULT_ROLE),
            canal:auth({kubernetes, Role, Token});
        Error ->
            Error
    end.

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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec set_secrets_error_test() -> _.
set_secrets_error_test() ->
    ?assertEqual(skip, set_secrets(error)).

-spec read_error_test() -> _.
read_error_test() ->
    ?assertEqual({error, enoent}, read_maybe_linked_file("unknown_file")).

-spec vault_auth_error_test() -> _.
vault_auth_error_test() ->
    ?assertEqual({error, enoent}, vault_client_auth("unknown_file")).

-endif.
