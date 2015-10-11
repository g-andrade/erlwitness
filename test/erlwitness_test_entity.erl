% vim: set expandtab softtabstop=4 shiftwidth=4:
-module(erlwitness_test_entity).
-author('Guilherme Andrade <erlwitness(at)gandrade(got)net>').

-behaviour(gen_server).

-export([start_link/2,
         start/2]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-record(state, {
        value :: term()
}).


start_link(Entity, EntityProcType) ->
    {WrappedArgs, StartOptions} = erlwitness:get_start_extras(Entity, EntityProcType),
    gen_server:start_link(?MODULE, WrappedArgs, StartOptions).

start(Entity, EntityProcType) ->
    {WrappedArgs, StartOptions} = erlwitness:get_start_extras(Entity, EntityProcType),
    gen_server:start(?MODULE, WrappedArgs, StartOptions).

init(WrappedArgs) ->
    [] = erlwitness:unwrap_init_args(WrappedArgs),
    erlwitness:finalize_init(WrappedArgs, {ok, #state{}}).

handle_call(die, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(change_state, State) ->
    {noreply, State#state{ value=os:timestamp() }};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


