% vim: set expandtab softtabstop=4 shiftwidth=4:
-module(erlwitness_conf).
-export([get_lookup_module/0,
         use_internal_indexing/0,
         get_index_serv_count/0]).

-spec get_lookup_module() -> module().
get_lookup_module() ->
    {ok, V} = application:get_env(erlwitness, entity_lookup_module),
    V.

-spec use_internal_indexing() -> boolean().
use_internal_indexing() ->
    get_lookup_module() == erlwitness_index_serv.

-spec get_index_serv_count() -> pos_integer().
get_index_serv_count() ->
    application:get_env(erlwitness, erlwitness_index_serv_count,
                        10 * erlang:system_info(schedulers_online)).
