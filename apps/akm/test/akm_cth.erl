-module(akm_cth).

-include_lib("common_test/include/ct.hrl").
-include_lib("bouncer_proto/include/bouncer_ctx_thrift.hrl").

-type config() :: [{atom(), term()}].
-type test_case_name() :: atom().
-type group_name() :: atom().

-export_type([config/0]).
-export_type([test_case_name/0]).
-export_type([group_name/0]).

%% API
-export([
    init/2,
    terminate/1,
    pre_init_per_suite/3
]).

-define(ACCESS_TOKEN, <<"some.access.token">>).
-define(AUTH_CTX, #{
    id => <<"auth_data_id">>,
    token => ?ACCESS_TOKEN,
    status => active,
    context => #ctx_ContextFragment{type = 'v1_thrift_binary'},
    metadata => #{
        <<"dev.vality.party.id">> => <<"some_party">>,
        <<"dev.vality.user.email">> => <<"box@mail.ru">>
    },
    authority => <<"authority_id">>
}).

-spec init(_, _) -> _.
init(_Id, State) ->
    pipe(
        [
            fun prepare_config/1,
            fun set_environment/1,
            fun mock_services/1,
            fun start_akm/1,
            fun start_gun/1
        ],
        State
    ).

%% every SUITE has it own config, so we have to inject params every time
-spec pre_init_per_suite(_, _, _) -> _.
pre_init_per_suite(_SuiteName, Config, State) ->
    {Config ++ State, State}.

-spec terminate(_) -> _.
terminate(_State) ->
    ok.

%% Internal functions

pipe(Funs, State) ->
    lists:foldl(fun(F, S) -> F(S) end, State, Funs).

set_environment(State) ->
    {_, SysConfig} = lookup_key(sys_config, State),
    lists:foreach(
        fun({Application, Config}) ->
            ok = application:load(Application),
            lists:foreach(
                fun({Param, Value}) ->
                    application:set_env(Application, Param, Value)
                end,
                Config
            )
        end,
        SysConfig
    ),
    State.

prepare_config(State) ->
    AkmAddress = "::",
    WorkDir = get_env_var("WORK_DIR"),
    AkmPort = get_free_port(),
    PgConfig = get_pg_config(),
    SysConfig = [
        {akm, [
            {ip, AkmAddress},
            {port, AkmPort},
            {transport, thrift},
            {bouncer_ruleset_id, <<"service/authz/api">>},
            {vault_token_path, WorkDir ++ "/rebar.config"},
            {health_check, #{
                disk => {erl_health, disk, ["/", 99]},
                memory => {erl_health, cg_memory, [99]},
                service => {erl_health, service, [<<"api-key-mgmt-v2">>]}
            }},
            {max_request_deadline, 60000},
            {epsql_connection, PgConfig},
            {auth_config, #{
                metadata_mappings => #{
                    party_id => <<"dev.vality.party.id">>,
                    user_id => <<"dev.vality.user.id">>,
                    user_email => <<"dev.vality.user.email">>
                }
            }},

            {mailer, #{
                url => "http://vality.dev",
                port => 465,
                from_email => "example@example.com",
                relay => "smtp4dev",
                password => "password",
                username => "username"
            }}
        ]},

        {canal, [
            {url, "http://vault:8200"},
            {engine, kvv2}
        ]}
    ],

    [
        {sys_config, SysConfig},
        {akm_host, "localhost"},
        {akm_port, AkmPort}
        | State
    ].

%%

mock_services(State) ->
    meck:expect(
        canal,
        auth,
        fun(_) -> ok end
    ),
    meck:expect(
        canal,
        read,
        fun(_) ->
            {
                ok,
                #{
                    <<"pg_creds">> => jsx:encode(#{
                        <<"pg_user">> => get_env_var("POSTGRES_USER", "postgres"),
                        <<"pg_password">> => get_env_var("POSTGRES_PASSWORD", "postgres")
                    })
                }
            }
        end
    ),
    meck:expect(
        bouncer_client,
        judge,
        fun(_, _, _) -> allowed end
    ),
    meck:expect(
        token_keeper_authority_offline,
        create,
        fun(_ID, _ContextFragment, _Metadata, _Client) ->
            {ok, #{token => ?ACCESS_TOKEN}}
        end
    ),
    meck:expect(
        token_keeper_authority_offline,
        revoke,
        fun(_ID, _Client) ->
            {ok, ok}
        end
    ),
    meck:expect(
        token_keeper_authenticator,
        authenticate,
        fun(_PreAuthContext, _TokenContext, _WoodyContext) ->
            {ok, ?AUTH_CTX}
        end
    ),
    meck:expect(
        token_keeper_client,
        offline_authority,
        fun(_, _) -> #{} end
    ),
    State.

start_akm(State) ->
    {ok, _} = application:ensure_all_started(akm),
    State.

start_gun(State) ->
    {ok, _} = application:ensure_all_started(gun),
    State.

get_free_port() ->
    {ok, Listen} = gen_tcp:listen(0, []),
    {ok, Port} = inet:port(Listen),
    ok = gen_tcp:close(Listen),
    Port.

get_pg_config() ->
    #{
        host => get_env_var("POSTGRES_HOST"),
        port => list_to_integer(get_env_var("POSTGRES_PORT", "5432")),
        %% username => get_env_var("POSTGRES_USER", "postgres"),
        %% password => get_env_var("POSTGRES_PASSWORD", "postgres"),
        database => get_env_var("POSTGRES_DB", "apikeymgmtv2")
    }.

get_env_var(Name) ->
    case os:getenv(Name) of
        false -> throw({os_env_required, Name});
        V -> V
    end.

get_env_var(Name, Default) ->
    os:getenv(Name, Default).

lookup_key(Key, Config) ->
    case lists:keyfind(Key, 1, Config) of
        false -> throw({config_key_required, Key});
        Tuple -> Tuple
    end.
