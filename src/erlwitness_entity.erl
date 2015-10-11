% vim: set expandtab softtabstop=4 shiftwidth=4:
-module(erlwitness_entity).
-export([set/1, get/0]).

-ignore_xref([{get, 0}]).

-define(PROCDIC_ENTITY, 'erlwitness/entity/identifier').

-spec set(Entity :: erlwitness:entity()) -> ok.
set(Entity) ->
    put(?PROCDIC_ENTITY, {value, Entity}),
    ok.

-spec get() -> {value, erlwitness:entity()} | undefined.
get() ->
    get(?PROCDIC_ENTITY).
