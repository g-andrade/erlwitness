% vim: set expandtab softtabstop=4 shiftwidth=4:
-module(erlwitness_app).
-author('Guilherme Andrade <erlwitness(at)(dot)net>').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    erlwitness_sup:start_link().

stop(_State) ->
    ok.
