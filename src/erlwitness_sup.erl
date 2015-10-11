% vim: set expandtab softtabstop=4 shiftwidth=4:
-module(erlwitness_sup).
-author('Guilherme Andrade <erlwitness(at)(dot)net>').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Indexers = case erlwitness_conf:use_internal_indexing() of
        false -> [];
        true ->
            [erlwitness_index_serv:child_spec(Id)
             || Id <- lists:seq(1, erlwitness_conf:get_index_serv_count())]
    end,
    AllChildren = [?CHILD(erlwitness_lobby, worker) | Indexers],
    {ok, {{one_for_one, 5, 10}, AllChildren}}.

