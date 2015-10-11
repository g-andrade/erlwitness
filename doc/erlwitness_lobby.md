

# Module erlwitness_lobby #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_server`](gen_server.md).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#is_entity_watched-1">is_entity_watched/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_entity_watched_by-2">is_entity_watched_by/2</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>
Starts the server.</td></tr><tr><td valign="top"><a href="#unwatch-2">unwatch/2</a></td><td></td></tr><tr><td valign="top"><a href="#unwatch_by_pid-1">unwatch_by_pid/1</a></td><td></td></tr><tr><td valign="top"><a href="#watch-2">watch/2</a></td><td></td></tr><tr><td valign="top"><a href="#watchers_local_lookup-1">watchers_local_lookup/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="is_entity_watched-1"></a>

### is_entity_watched/1 ###


<pre><code>
is_entity_watched(Entity::<a href="erlwitness.md#type-entity">erlwitness:entity()</a>) -&gt; boolean()
</code></pre>

<br></br>



<a name="is_entity_watched_by-2"></a>

### is_entity_watched_by/2 ###


<pre><code>
is_entity_watched_by(Entity::<a href="erlwitness.md#type-entity">erlwitness:entity()</a>, WatcherPid::pid()) -&gt; boolean()
</code></pre>

<br></br>



<a name="start_link-0"></a>

### start_link/0 ###


<pre><code>
start_link() -&gt; {ok, Pid} | ignore | {error, Error}
</code></pre>

<br></br>



Starts the server

<a name="unwatch-2"></a>

### unwatch/2 ###


<pre><code>
unwatch(Entity::<a href="erlwitness.md#type-entity">erlwitness:entity()</a>, WatcherPid::pid()) -&gt; ok
</code></pre>

<br></br>



<a name="unwatch_by_pid-1"></a>

### unwatch_by_pid/1 ###


<pre><code>
unwatch_by_pid(WatcherPid::pid()) -&gt; ok
</code></pre>

<br></br>



<a name="watch-2"></a>

### watch/2 ###


<pre><code>
watch(Entity::<a href="erlwitness.md#type-entity">erlwitness:entity()</a>, WatcherPid::pid()) -&gt; boolean()
</code></pre>

<br></br>



<a name="watchers_local_lookup-1"></a>

### watchers_local_lookup/1 ###


<pre><code>
watchers_local_lookup(Entity::<a href="erlwitness.md#type-entity">erlwitness:entity()</a>) -&gt; [pid()]
</code></pre>

<br></br>



