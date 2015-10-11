% vim: set expandtab softtabstop=4 shiftwidth=4:
-module(erlwitness_test_watcher).
-author('Guilherme Andrade <erlwitness(at)(dot)net>').

-behaviour(gen_server).
-behaviour(erlwitness_watcher).

-export([start_link/2,
         start/2]).

-export([init/1,
         handle_gencall_event/8,
         handle_gencast_event/7,
         handle_geninfo_event/7,
         handle_newstate_event/7,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-record(state, {
        destination_pid :: pid()
}).


start_link(Entities, FeedbackTestPid) ->
    erlwitness_watcher:start_link(Entities, ?MODULE, [FeedbackTestPid]).

start(Entities, FeedbackTestPid) ->
    erlwitness_watcher:start(Entities, ?MODULE, [FeedbackTestPid]).


init([FeedbackTestPid]) ->
    {ok, #state{ destination_pid=FeedbackTestPid }}.


handle_gencall_event(Timestamp, Entity, EntityPid, EntityProcType, EntityProcName, Call, From, State) ->
    handle_event(Timestamp, Entity, EntityPid, EntityProcType, EntityProcName, {call, Call, From}, State),
    {noreply, State}.

handle_gencast_event(Timestamp, Entity, EntityPid, EntityProcType, EntityProcName, Cast, State) ->
    handle_event(Timestamp, Entity, EntityPid, EntityProcType, EntityProcName, {cast, Cast}, State),
    {noreply, State}.

handle_geninfo_event(Timestamp, Entity, EntityPid, EntityProcType, EntityProcName, Info, State) ->
    handle_event(Timestamp, Entity, EntityPid, EntityProcType, EntityProcName, {info, Info}, State),
    {noreply, State}.

handle_newstate_event(Timestamp, Entity, EntityPid, EntityProcType, EntityProcName, EntityProcState, State) ->
    handle_event(Timestamp, Entity, EntityPid, EntityProcType, EntityProcName, {new_state, EntityProcState}, State),
    {noreply, State}.


handle_call(_Request, _From, State) ->
    {noreply, State}.


handle_cast(unwatch_all, State) ->
    ok = erlwitness_watcher:unwatch_all(),
    {noreply, State};

handle_cast({unwatch, Entity}, State) ->
    ok = erlwitness_watcher:unwatch(Entity),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_event(Timestamp, Entity, EntityPid, EntityProcType, EntityProcName, Event, State) ->
    %?debugFmt("got event: timestamp ~p, entity ~p, entity_pid ~p, entity_proc_type ~p, "
    %          "entity_proc_name ~p, event ~p~n",
    %          [Timestamp, Entity, EntityPid, EntityProcType, EntityProcName, Event]),
    State#state.destination_pid ! {EntityPid, EntityProcType, Event, self()}.
