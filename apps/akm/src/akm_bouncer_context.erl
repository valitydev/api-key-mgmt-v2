-module(akm_bouncer_context).

-include_lib("bouncer_proto/include/bouncer_ctx_v1_thrift.hrl").
-include_lib("bouncer_proto/include/bouncer_base_thrift.hrl").

-type fragment() :: bouncer_client:context_fragment().
-type acc() :: bouncer_context_helpers:context_fragment().

-type fragments() :: {acc(), _ExternalFragments :: #{_ID => fragment()}}.

-export_type([fragment/0]).
-export_type([acc/0]).
-export_type([fragments/0]).

-type operation_id() :: wapi_handler_utils:operation_id().
-type prototypes() :: [
    {operation, prototype_operation()}
    | {wallet, prototype_wallet()}
].

-type prototype_operation() :: #{
    id => operation_id(),
    party => maybe_undefined(entity_id()),
    identity => maybe_undefined(entity_id()),
    wallet => maybe_undefined(entity_id()),
    withdrawal => maybe_undefined(entity_id()),
    deposit => maybe_undefined(entity_id()),
    w2w_transfer => maybe_undefined(entity_id()),
    source => maybe_undefined(entity_id()),
    destination => maybe_undefined(entity_id()),
    report => maybe_undefined(entity_id()),
    file => maybe_undefined(entity_id()),
    webhook => maybe_undefined(entity_id())
}.

-type prototype_wallet() :: [wallet_entity()].

-type wallet_entity() ::
    {identity, identity_data()}
    | {wallet, wallet_data()}
    | {withdrawal, withdrawal_data()}
    | {deposit, deposit_data()}
    | {w2w_transfer, w2w_transfer_data()}
    | {source, source_data()}
    | {destination, destination_data()}
    | {webhook, webhook_data()}
    | {report, report_data()}.

-type wallet_entity_type() ::
    identity
    | wallet
    | withdrawal
    | deposit
    | w2w_transfer
    | source
    | destination
    | webhook
    | webhook_filter
    | report
    | report_file.

-type identity_data() :: #{
    id => entity_id()
}.

-type wallet_data() :: #{
    id => entity_id(),
    party => entity_id(),
    cash => cash()
}.

-type withdrawal_data() :: #{
    id => entity_id(),
    party => entity_id()
}.

-type deposit_data() :: #{
    id => entity_id(),
    party => entity_id()
}.

-type w2w_transfer_data() :: #{
    id => entity_id(),
    party => entity_id()
}.

-type source_data() :: #{
    id => entity_id(),
    party => entity_id()
}.

-type destination_data() :: #{
    id => entity_id(),
    party => entity_id()
}.

-type webhook_data() :: #{
    id => entity_id(),
    identity => entity_id(),
    wallet => entity_id()
}.

-type report_data() :: #{
    id => entity_id(),
    identity => entity_id(),
    files => [entity_id()]
}.

-type entity_id() :: binary().
-type maybe_undefined(Type) :: Type | undefined.
-type cash() :: #{amount := binary(), currency := binary()}.

-export_type([prototypes/0]).
-export_type([prototype_operation/0]).
-export_type([prototype_wallet/0]).
-export_type([wallet_entity_type/0]).

-export([new/0]).
-export([build/2]).

%%

-spec new() -> fragments().
new() ->
    {mk_base_fragment(), #{}}.

mk_base_fragment() ->
    bouncer_context_helpers:make_env_fragment(#{
        now => genlib_rfc3339:format(genlib_time:unow(), second),
        deployment => #{id => genlib_app:env(wapi_lib, deployment, undefined)}
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
                party = maybe_entity(party_id, Params),
                api_key = maybe(api_key, Params)
            }
        }
    }.

%%

maybe(_Name, undefined) ->
    undefined;
maybe(Name, Params) ->
    maps:get(Name, Params, undefined).

maybe_entity(_Name, undefined) ->
    undefined;
maybe_entity(Name, Params) ->
    case maps:get(Name, Params, undefined) of
        undefined ->
            undefined;
        Value ->
            #base_Entity{id = Value}
    end.

operation_id_to_binary(V) ->
    erlang:atom_to_binary(V, utf8).
