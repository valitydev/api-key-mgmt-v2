-module(akm_apikeys_processing).

-export([issue_api_key/2]).

issue_api_key(PartyID, ApiKey) ->
    #{
        <<"name">> := Name,
        <<"metadata">> := Metadata
    } = ApiKey,
%%  REWORK ненормальный ID, переработать
    ID = akm_id:generate_snowflake_id(),
    Status = "active",
    {ok, 1} = epgsql_pool:query(
        main_query,
        "INSERT INTO apikeys (id, name, party_id, status, metadata) VALUES ($1, $2, $3, $4, $5)",
        [ID, Name, PartyID, Status, Metadata]
    ).

