% vim: set expandtab softtabstop=4 shiftwidth=4:
-module(erlwitness_entity).
-export([set_params/3, get_entity/0, report_lager_event/8]).

-ignore_xref([{get_entity, 0}, {report_lager_event, 8}]).

%% The following code should only be used internally / through the parse transform.

-define(PROCDIC_ENTITY_IDENTIFIER, 'erlwitness/entity/identifier').
-define(PROCDIC_ENTITY_PROCTYPE, 'erlwitness/entity/proc_type').
-define(PROCDIC_ENTITY_PROCNAME, 'erlwitness/entity/proc_name').

-spec set_params(Entity :: erlwitness:entity(),
                 EntityProcType :: erlwitness:process_type(),
                 EntityProcName :: term()) -> ok.
set_params(Entity, EntityProcType, EntityProcName) ->
    put(?PROCDIC_ENTITY_IDENTIFIER, {value, Entity}),
    put(?PROCDIC_ENTITY_PROCTYPE, EntityProcType),
    PrevProcName = get(?PROCDIC_ENTITY_PROCNAME),
    case is_pid(PrevProcName) orelse PrevProcName == undefined of
        false -> ok;
        true  ->
            put(?PROCDIC_ENTITY_PROCNAME, EntityProcName),
            ok
    end.

-spec get_entity() -> {value, erlwitness:entity()} | undefined.
get_entity() ->
    get(?PROCDIC_ENTITY_IDENTIFIER).

-spec report_lager_event(Watchers :: [pid()], Entity :: erlwitness:entity(),
                         LagerModule :: module(), LagerFunction :: atom(), LagerArgs :: list(),
                         CodeModule :: module(), CodeFunction :: atom(), CodeLine :: pos_integer()) -> ok.
report_lager_event(Watchers, Entity, LagerModule, LagerFunction, LagerArgs,
                   CodeModule, CodeFunction, CodeLine) ->
    Timestamp = os:timestamp(),
    EntityPid = self(),
    EntityProcType = get(?PROCDIC_ENTITY_PROCTYPE),
    EntityProcName = get(?PROCDIC_ENTITY_PROCNAME),
    lists:foreach(
        fun (Watcher) ->
                ok = erlwitness_watcher:report_lager_event(Watcher, Timestamp, Entity, EntityPid,
                                                           EntityProcType, EntityProcName,
                                                           LagerModule, LagerFunction, LagerArgs,
                                                           CodeModule, CodeFunction, CodeLine)
        end,
        Watchers).
