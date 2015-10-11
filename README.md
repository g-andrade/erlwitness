

# erlwitness #

Copyright (c) 2015 Guilherme Andrade

__Version:__ 1.0.0

__Authors:__ Guilherme Andrade ([`erlwitness(at)gandrade(dot)net`](mailto:erlwitness(at)gandrade(dot)net)).

`erlwitness`: On-demand semantic process group watchtower 

---------


### <a name="What_does_it_do?">What does it do?</a> ###


`erlwitness` allows one to conveniently funnel `gen_server` events (init, calls, casts, info, state changes) specific to one or more entities (user_id, person_name, whatever you want to use) into arbitrary watcher processes.


### <a name="How_do_I_use_it?">How do I use it?</a> ###


There are two main parts to an ordinary setup:

1) Adapt your existing gen_server:start / gen_server:start_link calls to make use of the relevant registration;

```erlang

% ....
start_link(Person, Files, LuckyNumber) ->
    {WrappedArgs, StartOptions} = erlwitness:get_start_extras(Person, person_file_serv,
                                                              [Files, LuckyNumber]),
    gen_server:start_link(?MODULE, WrappedArgs, StartOptions).

init(WrappedArgs) ->
    [Files, LuckyNumber] = erlwitness:unwrap_init_args(WrappedArgs),
    InitResult = {ok, #state{files = Files,
                             lucky_number = LuckyNumber}},
    erlwitness:finalize_init(WrappedArgs, InitResult).
% ....

```

2) Code your own watcher process which will implement both `gen_server` and `erlwitness_watcher` behaviours.

```erlang

% ...
start_link(Person) ->
    erlwitness_watcher:start_link([Person], ?MODULE, []).

% ...

handle_gencall_event(Timestamp, Entity, EntityPid, EntityProcType, 
                     EntityProcName, Call, From, State) ->
    io:format("I watch everything: ~p~n", [{Timestamp, Entity, EntityPid, 
                                            EntityProcType, Call, From}]),
    {noreply, State}.

% ..etc.

```



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

