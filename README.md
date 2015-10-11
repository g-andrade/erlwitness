

# erlwitness #

Copyright (c) 2015 Guilherme Andrade

__Version:__ 1.0.0

__Authors:__ Guilherme Andrade ([`erlwitness(at)gandrade(dot)net`](mailto:erlwitness(at)gandrade(dot)net)).

`erlwitness`: Semantic process groups watchtower.

---------


### <a name="What_does_it_do?">What does it do?</a> ###


`erlwitness` allows one to funnel `gen_server` events (init, calls, casts, infos, state changes) specific to identified entities into arbitrary watcher processes.


### <a name="Why?">Why?</a> ###


For example: remote activity tracers for sizeable software development teams (instead of _hey, may you please check the log?_)


### <a name="Main_features">Main features</a> ###


- Multiple watchers can spy on the same entity;
- A single watcher can spy on multiple entities;
- An entity can consist of zero or more processes that come and go;
- Watching works transparently on both local and remote nodes;
- Watching can be initiated both before and after entity processes have been spawned.


### <a name="How_do_I_use_it?">How do I use it?</a> ###


There are two main parts to an ordinary setup:

1) Adapt your existing entities' gen_server:start / gen_server:start_link / init calls to make use of the relevant registration flow:

```erlang

% ....
-behaviour(gen_server).
% ...

start_link(Person, Files, LuckyNumber) ->
    {WrappedArgs, StartOptions} = erlwitness:get_start_extras({person, Person}, 
                                                              person_file_serv,
                                                              [Files, LuckyNumber]),
    gen_server:start_link(?MODULE, WrappedArgs, StartOptions).

init(WrappedArgs) ->
    [Files, LuckyNumber] = erlwitness:unwrap_init_args(WrappedArgs),
    InitResult = {ok, #state{files = Files,
                             lucky_number = LuckyNumber}},
    erlwitness:finalize_init(WrappedArgs, InitResult).
% ....

```

2) Code your own watcher process which will implement both `gen_server` and `erlwitness_watcher` behaviours:

```erlang

% ...
-behaviour(gen_server).
-behaviour(erlwitness_watcher).
% ...

start_link(Person) ->
    erlwitness_watcher:start_link([{person, Person}], ?MODULE, []).

% ...

handle_gencall_event(Timestamp, {person, Person}, PersonPid, PersonProcType,
                     PersonProcName, Call, From, State) ->
    io:format("Here's a call: ~p~n", [{Timestamp, Person, PersonPid,
                                       PersonProcType, Call, From}]),
    {noreply, State}.

handle_gencast_event(Timestamp, {person, Person}, PersonPid, PersonProcType,
                     PersonProcName, Cast, State) ->
    io:format("Here's a cast: ~p~n", [{Timestamp, Person, PersonPid,
                                       PersonProcType, Cast}]),
    {noreply, State}.

handle_geninfo_event(Timestamp, {person, Person}, PersonPid, PersonProcType,
                     PersonProcName, Info, State) ->
    io:format("Here's an info: ~p~n", [{Timestamp, Person, PersonPid,
                                       PersonProcType, Info}]),
    {noreply, State}.

handle_newstate_event(Timestamp, {person, Person}, PersonPid, PersonProcType,
                      PersonProcName, PersonState, State) ->
    io:format("Here's a new state: ~p~n", [{Timestamp, Person, PersonPid,
                                            PersonProcType, Info}]),
    {noreply, State}.
% ..

```

.. and optionally, 3) Reuse your existing entity registration code by implementing the `erlwitness_lookup` behaviour and adjusting `erlwitness` app.config acordingly.

```erlang

% ...
-behaviour(erlwitness_lookup).
% ...

lookup_global_entity({person, Person}) ->
   PeopleInfo = mnesia:dirty_read(person_info, Person),
   [{person_file_serv, PersonInfo#person_info.file_serv_pid} || PersonInfo <- Pids].
% ...

```

Full implementations under _example/_.


### <a name="What_about_the_overhead?">What about the overhead?</a> ###


This software is built on the premise that watchers are few, rare, and mostly limited
to development environments; therefore watching is heavy and privacy is light.
- Unwatched processes: basic ETS lookup just before the process starts;
- Watched processes: one OTP debug fun per watcher (see [sys(3)](http://www.erlang.org/doc/man/sys.html)).


### <a name="Why_are_there_no_termination_events?">Why are there no termination events?</a> ###


This software is a building block; tracking and monitoring entity processes on a watcher is left as an exercise for the reader.


### <a name="What's_the_deal_with_entity_registration?">What's the deal with entity registration?</a> ###


`erlwitness_index_serv` is bundled as an out-of-the-box solution; it might not, however, suit your needs. Please mind that it will, for each spawned entity:
- 1) Write a new entry to an ETS table (on `erlwitness:finalize_init/2`);
- 2) Create a monitor from a specific worker on the indexing pool;
- 3) Trigger the monitor on termination in order to unregister the entity.

In order to lax this potential bottleneck, the indexing pool will spawn (_10 x NumberOfSchedulers_) monitoring processes by default, with one separate ETS table per indexer, and NumberOfSchedulers generally (but not always) corresponding to the number of CPU thread queues (usually _NumOfCPUs x NumOfCoresPerCPU x (1 + HyperthreadPerCore)_).

However, if you already have your own global process directory in place, it's recommended that you use it instead.


### <a name="Future_plans">Future plans</a> ###

- Some sort of lager /error\_logger per-entity sink;
- Offline watcher scheduling with later history retrieving.


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="https://github.com/g-andrade/erlwitness/blob/master/doc/erlwitness.md" class="module">erlwitness</a></td></tr>
<tr><td><a href="https://github.com/g-andrade/erlwitness/blob/master/doc/erlwitness_app.md" class="module">erlwitness_app</a></td></tr>
<tr><td><a href="https://github.com/g-andrade/erlwitness/blob/master/doc/erlwitness_conf.md" class="module">erlwitness_conf</a></td></tr>
<tr><td><a href="https://github.com/g-andrade/erlwitness/blob/master/doc/erlwitness_index_serv.md" class="module">erlwitness_index_serv</a></td></tr>
<tr><td><a href="https://github.com/g-andrade/erlwitness/blob/master/doc/erlwitness_lobby.md" class="module">erlwitness_lobby</a></td></tr>
<tr><td><a href="https://github.com/g-andrade/erlwitness/blob/master/doc/erlwitness_lookup.md" class="module">erlwitness_lookup</a></td></tr>
<tr><td><a href="https://github.com/g-andrade/erlwitness/blob/master/doc/erlwitness_sup.md" class="module">erlwitness_sup</a></td></tr>
<tr><td><a href="https://github.com/g-andrade/erlwitness/blob/master/doc/erlwitness_watcher.md" class="module">erlwitness_watcher</a></td></tr></table>

