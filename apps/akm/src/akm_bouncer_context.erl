-module(akm_bouncer_context).

-include_lib("bouncer_proto/include/bouncer_ctx_v1_thrift.hrl").
-include_lib("bouncer_proto/include/bouncer_base_thrift.hrl").

-type fragment() :: bouncer_client:context_fragment().
-type acc() :: bouncer_context_helpers:context_fragment().

-type fragments() :: {acc(), _ExternalFragments :: #{_ID => fragment()}}.

-export_type([fragment/0]).
-export_type([acc/0]).
-export_type([fragments/0]).

-type operation_id() :: akm_handler_utils:operation_id().
-type prototypes() :: [
    {operation, prototype_operation()}
].

-type prototype_operation() :: #{
    id => operation_id(),
    party => maybe_undefined(entity_id()),
    api_key => maybe_undefined(entity_id())
}.

-type entity_id() :: binary().
-type maybe_undefined(Type) :: Type | undefined.

-export_type([prototypes/0]).
-export_type([prototype_operation/0]).

-export([new/0]).
-export([build/2]).

%%

-spec new() -> fragments().
new() ->
    {mk_base_fragment(), #{}}.

mk_base_fragment() ->
    bouncer_context_helpers:make_env_fragment(#{
        now => genlib_rfc3339:format(genlib_time:unow(), second),
        deployment => #{id => genlib_app:env(akm, deployment, undefined)}
    }).

-spec build(prototypes(), fragments()) -> fragments().
build(Prototypes, {Acc0, External}) ->
    Acc1 = lists:foldl(fun({T, Params}, Acc) -> build(T, Params, Acc) end, Acc0, Prototypes),
    {Acc1, External}.

build(operation, Params = #{id := OperationID}, Acc) ->
    PartyEntity = party_entity(Params),
    ApiKeyEntity = api_key_entity(Params),
    ListEntities = lists:filter(fun(E) -> E =/= undefined end, [PartyEntity, ApiKeyEntity]),
    Ctx = Acc#ctx_v1_ContextFragment{
        apikeymgmt = #ctx_v1_ContextApiKeyMgmt{
            op = #ctx_v1_ApiKeyMgmtOperation{
                id = operation_id_to_binary(OperationID),
                party = PartyEntity,
                api_key = ApiKeyEntity
            }
        }
    },
    maybe_add_entities(Ctx, ListEntities).

%%

api_key_entity(
    #{
        api_key := #{
            <<"id">> := ApiKeyId,
            <<"metadata">> := MetaData
        }
    }
) ->
    PartyId = akm_auth:get_party_from_metadata(MetaData),
    #base_Entity{id = ApiKeyId, party = PartyId, type = <<"ApiKey">>};
api_key_entity(_) ->
    undefined.

party_entity(#{party := PartyId}) ->
    #base_Entity{id = PartyId};
party_entity(_) ->
    undefined.

operation_id_to_binary(V) ->
    erlang:atom_to_binary(V, utf8).

maybe_add_entities(Ctx, []) ->
    Ctx;
maybe_add_entities(Ctx, ListEntities) ->
    Ctx#ctx_v1_ContextFragment{
        entities = ordsets:from_list(ListEntities)
    }.
