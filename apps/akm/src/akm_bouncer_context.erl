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
    Acc#ctx_v1_ContextFragment{
        apikeymgmt = #ctx_v1_ContextApiKeyMgmt{
            op = #ctx_v1_ApiKeyMgmtOperation{
                id = operation_id_to_binary(OperationID),
                party = party_entity(Params),
                api_key = api_key_entity(Params)
            }
        }
    }.

%%

%maybe(Name, Params) ->
%    maps:get(Name, Params, undefined).

api_key_entity(#{api_key := ApiKeyId, party := PartyId}) ->
    #base_Entity{id = ApiKeyId, party = PartyId};
api_key_entity(_) ->
    undefined.

party_entity(#{party := PartyId}) ->
    #base_Entity{id = PartyId};
party_entity(_) ->
    undefined.

operation_id_to_binary(V) ->
    erlang:atom_to_binary(V, utf8).
