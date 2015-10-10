% vim: set expandtab softtabstop=4 shiftwidth=4:
-module(erlwitness).
-export([get_entity_dbg_options/2, index_entity/3]).
-export_type([entity/0, process_type/0]).

-ignore_xref([{get_entity_dbg_options, 2}, {index_entity, 3}]).

-type entity() :: any().
-type process_type() :: any().

-spec get_entity_dbg_options(Entity :: entity(), EntityProcType :: process_type())
        -> [{install, {erlwitness_watcher:dbg_fun(), erlwitness_watcher:dbg_fun_state()}}].
get_entity_dbg_options(Entity, EntityProcType) ->
    erlwitness_watcher:get_entity_dbg_options(Entity, EntityProcType).

-spec index_entity(Entity :: entity(), EntityProcType :: process_type(), EntityPid :: pid())
        -> ok | {error, not_using_internal_indexing}.
index_entity(Entity, EntityProcType, EntityPid) ->
    case erlwitness_conf:use_internal_indexing() of
        false -> {error, not_using_internal_indexing};
        true ->
            ok = erlwitness_index_serv:register_entity(Entity, EntityProcType, EntityPid)
    end.
