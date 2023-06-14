-module(akm_apikeys_processing).

-include_lib("bouncer_proto/include/bouncer_ctx_v1_thrift.hrl").

-export([issue_api_key/3]).

issue_api_key(PartyID, ApiKey, WoodyContext) ->
    #{
        <<"name">> := Name,
        <<"metadata">> := Metadata
    } = ApiKey,
%%  REWORK ненормальный ID, переработать
    ID = akm_id:generate_snowflake_id(),
    ContextFragment = bouncer_context_helpers:make_auth_fragment(#{
        method => <<"IssueApiKey">>,
        scope => [#{party => #{id => PartyID}}],
        token => #{id => ID}
    }),
    Status = "active",
    Metadata = #{
        <<"party.id">> => PartyID
    },
    Client = token_keeper_client:offline_authority(get_authority_id(), WoodyContext),
    case token_keeper_authority_offline:create(ID, ContextFragment, Metadata, Client) of
        {ok, #{token := Token}} ->
            {ok, 1} = epgsql_pool:query(
                main_query,
                "INSERT INTO apikeys (id, name, party_id, status, metadata) VALUES ($1, $2, $3, $4, $5)",
                [ID, Name, PartyID, Status, Metadata]
            ),
            #{
                id => ID,
                name => Name,
                party_id => PartyID,
                status => Status,
                metadata => Metadata,
                token => Token
            };
        {error, {auth_data, already_exists}} ->
            {error, already_exists}
    end.

get_authority_id() ->
    application:get_env(akm, authority_id).
