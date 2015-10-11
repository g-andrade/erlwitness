% vim: set expandtab softtabstop=4 shiftwidth=4:
-module(erlwitness_lookup).
-author('Guilherme Andrade <erlwitness(at)(dot)net>').

-export_type([indexed_entity_ref/0]).
-ignore_xref([behaviour_info/1]).

-type indexed_entity_ref() :: {ProcType :: erlwitness:process_type(), Pid :: pid()}.

-callback lookup_global_entity(Entity :: erlwitness:entity()) -> [indexed_entity_ref()].
