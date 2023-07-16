-module(akm_ct_utils).

%% API
-export([
    lookup_config/2,
    lookup_config/3,
    cleanup_db/0
]).

-spec lookup_config(_, _) -> _.
lookup_config(Key, Config) ->
    case lists:keyfind(Key, 1, Config) of
        false -> throw({config_key_required, Key});
        {_, Value} -> Value
    end.

-spec lookup_config(_, _, _) -> _.
lookup_config(Key, Config, Default) ->
    case lists:keyfind(Key, 1, Config) of
        false -> Default;
        {_, Value} -> Value
    end.

-spec cleanup_db() -> ok.
cleanup_db() ->
    {ok, _, _} = epgsql_pool:query(main_pool, "TRUNCATE apikeys"),
    ok.
