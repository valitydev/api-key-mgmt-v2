-module(akm_basic_test_SUITE).

%% API
-export([
    init_per_suite/1,
    end_per_suite/1,
    all/0
]).

-export([issue_get_key_success_test/1]).

%% also defined in ct hook module akm_cth.erl
-define(ACCESS_TOKEN, <<"some.access.token">>).

-spec init_per_suite(_) -> _.
init_per_suite(Config) ->
    Config.

-spec end_per_suite(_) -> _.
end_per_suite(_Config) ->
    ok.

-spec all() -> list().
all() ->
    [
        issue_get_key_success_test
    ].

-spec issue_get_key_success_test(_) -> _.
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
