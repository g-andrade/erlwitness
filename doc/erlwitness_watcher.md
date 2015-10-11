

# Module erlwitness_watcher #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_server`](gen_server.md).

__This module defines the `erlwitness_watcher` behaviour.__
<br></br>
 Required callback functions: `handle_gencall_event/8`, `handle_gencast_event/7`, `handle_geninfo_event/7`, `handle_newstate_event/7`.

<a name="types"></a>

## Data Types ##




### <a name="type-dbg_fun">dbg_fun()</a> ###



<pre><code>
dbg_fun() = fun((FuncState::<a href="#type-dbg_fun_state">dbg_fun_state()</a>, Event::any(), ProcName::any()) -&gt; NewFuncState::<a href="#type-dbg_fun_state">dbg_fun_state()</a>)
</code></pre>





### <a name="type-dbg_fun_state">dbg_fun_state()</a> ###



<pre><code>
dbg_fun_state() = active | done
</code></pre>





### <a name="type-handler_return">handler_return()</a> ###



<pre><code>
handler_return() = {noreply, NewState::term()} | {noreply, NewState::term(), timeout() | hibernate} | {stop, Reason::term(), NewState::term()}
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_entity_dbg_options-3">get_entity_dbg_options/3</a></td><td></td></tr><tr><td valign="top"><a href="#install_dbg_fun-4">install_dbg_fun/4</a></td><td></td></tr><tr><td valign="top"><a href="#report_init-7">report_init/7</a></td><td></td></tr><tr><td valign="top"><a href="#start-3">start/3</a></td><td></td></tr><tr><td valign="top"><a href="#start-4">start/4</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-3">start_link/3</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-4">start_link/4</a></td><td></td></tr><tr><td valign="top"><a href="#unwatch-1">unwatch/1</a></td><td></td></tr><tr><td valign="top"><a href="#unwatch_all-0">unwatch_all/0</a></td><td></td></tr><tr><td valign="top"><a href="#watch-1">watch/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get_entity_dbg_options-3"></a>

### get_entity_dbg_options/3 ###


<pre><code>
get_entity_dbg_options(Entity::<a href="erlwitness.md#type-entity">erlwitness:entity()</a>, EntityProcType::<a href="erlwitness.md#type-process_type">erlwitness:process_type()</a>, Watchers::[pid()]) -&gt; [{install, {<a href="#type-dbg_fun">dbg_fun()</a>, <a href="#type-dbg_fun_state">dbg_fun_state()</a>}}]
</code></pre>

<br></br>



<a name="install_dbg_fun-4"></a>

### install_dbg_fun/4 ###


<pre><code>
install_dbg_fun(Entity::<a href="erlwitness.md#type-entity">erlwitness:entity()</a>, EntityProcType::<a href="erlwitness.md#type-process_type">erlwitness:process_type()</a>, EntityPid::pid(), Watcher::pid()) -&gt; any()
</code></pre>

<br></br>



<a name="report_init-7"></a>

### report_init/7 ###

`report_init(Watcher, Timestamp, Entity, EntityPid, EntityProcType, EntityProcName, EntityProcState) -> any()`


<a name="start-3"></a>

### start/3 ###

`start(Entities, WatcherModule, WatcherArgs) -> any()`


<a name="start-4"></a>

### start/4 ###

`start(Name, Entities, WatcherModule, WatcherArgs) -> any()`


<a name="start_link-3"></a>

### start_link/3 ###

`start_link(Entities, WatcherModule, WatcherArgs) -> any()`


<a name="start_link-4"></a>

### start_link/4 ###

`start_link(Name, Entities, WatcherModule, WatcherArgs) -> any()`


<a name="unwatch-1"></a>

### unwatch/1 ###


<pre><code>
unwatch(Entity::<a href="erlwitness.md#type-entity">erlwitness:entity()</a>) -&gt; ok
</code></pre>

<br></br>



<a name="unwatch_all-0"></a>

### unwatch_all/0 ###


<pre><code>
unwatch_all() -&gt; ok
</code></pre>

<br></br>



<a name="watch-1"></a>

### watch/1 ###


<pre><code>
watch(Entity::<a href="erlwitness.md#type-entity">erlwitness:entity()</a>) -&gt; ok
</code></pre>

<br></br>



