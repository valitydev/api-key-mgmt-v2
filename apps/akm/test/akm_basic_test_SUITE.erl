-module(akm_basic_test_SUITE).

%% API
-export([
    init_per_suite/1,
    end_per_suite/1,
    all/0,
    groups/0
]).

-export([issue_get_key_success_test/1]).
-export([get_unknown_key_test/1]).
-export([list_keys_test/1]).
-export([revoke_key_test/1]).

%% also defined in ct hook module akm_cth.erl
-define(ACCESS_TOKEN, <<"some.access.token">>).

-type config() :: akm_cth:config().
-type test_case_name() :: akm_cth:test_case_name().
-type group_name() :: akm_cth:group_name().
-type test_result() :: any() | no_return().

-spec init_per_suite(_) -> _.
init_per_suite(Config) ->
    Config.

-spec end_per_suite(_) -> _.
end_per_suite(_Config) ->
    ok = akm_ct_utils:cleanup_db(),
    ok.

-spec all() -> [{group, test_case_name()}].
all() ->
    [{group, basic_operations}].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {basic_operations, [], [
            issue_get_key_success_test,
            get_unknown_key_test,
            list_keys_test,
            revoke_key_test
        ]}
    ].

-spec issue_get_key_success_test(config()) -> test_result().
issue_get_key_success_test(Config) ->
    Host = akm_ct_utils:lookup_config(akm_host, Config),
    Port = akm_ct_utils:lookup_config(akm_port, Config),
    ApiKeyIssue = #{
        name => <<"live-site-integration">>,
        metadata => #{
            key => <<"value">>
        }
    },
    PartyId = <<"test_party">>,
    #{
        <<"AccessToken">> := #{<<"accessToken">> := ?ACCESS_TOKEN},
        <<"ApiKey">> := #{
            <<"createdAt">> := _DateTimeRfc3339,
            <<"id">> := ApiKeyId,
            <<"metadata">> := #{
                <<"key">> := <<"value">>,
                <<"party.id">> := <<"test_party">>
            },
            <<"name">> := <<"live-site-integration">>,
            <<"status">> := <<"active">>
        } = ExpectedApiKey
    } = akm_client:issue_key(Host, Port, PartyId, ApiKeyIssue),

    %% check getApiKey
    ExpectedApiKey = akm_client:get_key(Host, Port, PartyId, ApiKeyId).

-spec get_unknown_key_test(config()) -> test_result().
get_unknown_key_test(Config) ->
    Host = akm_ct_utils:lookup_config(akm_host, Config),
    Port = akm_ct_utils:lookup_config(akm_port, Config),
    PartyId = <<"unknown_key_test_party">>,
    not_found = akm_client:get_key(Host, Port, PartyId, <<"UnknownKeyId">>).

-spec list_keys_test(config()) -> test_result().
list_keys_test(Config) ->
    Host = akm_ct_utils:lookup_config(akm_host, Config),
    Port = akm_ct_utils:lookup_config(akm_port, Config),
    PartyId = <<"list_test_party">>,

    %% check empty list
    #{<<"results">> := []} = akm_client:list_keys(Host, Port, PartyId),

    ListKeys = lists:foldl(
        fun(Num, Acc) ->
            #{<<"ApiKey">> := ApiKey} = akm_client:issue_key(
                Host,
                Port,
                PartyId,
                #{name => <<(erlang:integer_to_binary(Num))/binary, "list_keys_success">>}
            ),
            [ApiKey | Acc]
        end,
        [],
        lists:seq(1, 10)
    ),
    ExpectedList = lists:reverse(ListKeys),

    %% check one batch
    #{
        <<"results">> := ExpectedList
    } = akm_client:list_keys(Host, Port, PartyId),

    %% check continuation when limit multiple of the count keys
    MultLimit = <<"1">>,
    ExpectedList = get_list_keys(
        Host,
        Port,
        PartyId,
        MultLimit,
        akm_client:list_keys(Host, Port, PartyId, [{<<"limit">>, MultLimit}]),
        []
    ),

    %% check continuation when limit NOT multiple of the count keys
    NoMultLimit = <<"3">>,
    ExpectedList = get_list_keys(
        Host,
        Port,
        PartyId,
        NoMultLimit,
        akm_client:list_keys(Host, Port, PartyId, [{<<"limit">>, NoMultLimit}]),
        []
    ).

-spec revoke_key_test(config()) -> test_result().
revoke_key_test(Config) ->
    Host = akm_ct_utils:lookup_config(akm_host, Config),
    Port = akm_ct_utils:lookup_config(akm_port, Config),
    PartyId = <<"revoke_party">>,

    #{
        <<"ApiKey">> := #{
            <<"id">> := ApiKeyId
        }
    } = akm_client:issue_key(Host, Port, PartyId, #{name => <<"live-site-integration">>}),

    Result = akm_client:revoke_key(Host, Port, PartyId, ApiKeyId),
    io:format(user, "RES: ~p~n", [Result]).

get_list_keys(Host, Port, PartyId, Limit, #{<<"results">> := ListKeys, <<"continuationToken">> := Cont}, Acc) ->
    Params = [{<<"limit">>, Limit}, {<<"continuationToken">>, Cont}],
    get_list_keys(Host, Port, PartyId, Limit, akm_client:list_keys(Host, Port, PartyId, Params), Acc ++ ListKeys);
get_list_keys(_Host, _Port, _PartyId, _Limit, #{<<"results">> := ListKeys}, Acc) ->
    Acc ++ ListKeys.
