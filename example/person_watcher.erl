% vim: set expandtab softtabstop=4 shiftwidth=4:
-module(person_watcher).
-author('Guilherme Andrade <erlwitness(at)gandrade(dot)net>').

-behaviour(gen_server).
-behaviour(erlwitness_watcher).

-export([start_link/1,
         start/1]).

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


-record(state, {
        event_count = 0 :: non_neg_integer()
}).


start_link(Person) ->
    erlwitness_watcher:start_link([{person, Person}], ?MODULE, []).

start(Person) ->
    erlwitness_watcher:start([{person, Person}], ?MODULE, []).


init([]) ->
    {ok, #state{}}.


handle_gencall_event(Timestamp, {person, Person}, PersonPid, person_file_serv=PersonProcType, PersonProcName, Call, From, #state{}=State) ->
    handle_event(Timestamp, {person, Person}, PersonPid, PersonProcType, PersonProcName, {call, Call, From}, State),
    {noreply, State}.

handle_gencast_event(Timestamp, {person, Person}, PersonPid, person_file_serv=PersonProcType, PersonProcName, Cast, #state{}=State) ->
    handle_event(Timestamp, {person, Person}, PersonPid, PersonProcType, PersonProcName, {cast, Cast}, State),
    {noreply, State}.

handle_geninfo_event(Timestamp, {person, Person}, PersonPid, person_file_serv=PersonProcType, PersonProcName, Info, #state{}=State) ->
    handle_event(Timestamp, {person, Person}, PersonPid, PersonProcType, PersonProcName, {info, Info}, State),
    {noreply, State}.

handle_newstate_event(Timestamp, {person, Person}, PersonPid, person_file_serv=PersonProcType, PersonProcName, PersonProcState, #state{}=State) ->
    handle_event(Timestamp, {person, Person}, PersonPid, PersonProcType, PersonProcName, {new_state, PersonProcState}, State),
    {noreply, State}.


handle_call(_Request, _From, #state{}=State) ->
    {noreply, State}.


handle_cast(_Msg, #state{}=State) ->
    {noreply, State}.


handle_info(_Info, #state{}=State) ->
    {noreply, State}.


terminate(_Reason, #state{}) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_event(Timestamp, {person, Person}, PersonPid, person_file_serv=PersonProcType, PersonProcName, Event, #state{}=State) ->
    io:format("example watcher received event #~p: "
              "timestamp ~p, person ~p, entity_pid ~p, entity_proc_type ~p, "
              "entity_proc_name ~p, event ~p~n",
              [State#state.event_count,
               Timestamp, Person, PersonPid, PersonProcType, PersonProcName, Event]),
    State#state{ event_count = State#state.event_count + 1 }.
