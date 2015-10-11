

# Module erlwitness_index_serv #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`erlwitness_lookup`](erlwitness_lookup.md), [`gen_server`](gen_server.md).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#child_spec-1">child_spec/1</a></td><td></td></tr><tr><td valign="top"><a href="#lookup_entity-1">lookup_entity/1</a></td><td></td></tr><tr><td valign="top"><a href="#lookup_global_entity-1">lookup_global_entity/1</a></td><td></td></tr><tr><td valign="top"><a href="#register_entity-3">register_entity/3</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td>
Starts the server.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="child_spec-1"></a>

### child_spec/1 ###


<pre><code>
child_spec(Id::pos_integer()) -&gt; <a href="supervisor.md#type-child_spec">supervisor:child_spec()</a>
</code></pre>

<br></br>



<a name="lookup_entity-1"></a>

### lookup_entity/1 ###


<pre><code>
lookup_entity(Entity::<a href="erlwitness.md#type-entity">erlwitness:entity()</a>) -&gt; [<a href="erlwitness_lookup.md#type-indexed_entity_ref">erlwitness_lookup:indexed_entity_ref()</a>]
</code></pre>

<br></br>



<a name="lookup_global_entity-1"></a>

### lookup_global_entity/1 ###


<pre><code>
lookup_global_entity(Entity::<a href="erlwitness.md#type-entity">erlwitness:entity()</a>) -&gt; [<a href="erlwitness_lookup.md#type-indexed_entity_ref">erlwitness_lookup:indexed_entity_ref()</a>]
</code></pre>

<br></br>



<a name="register_entity-3"></a>

### register_entity/3 ###


<pre><code>
register_entity(Entity::<a href="erlwitness.md#type-entity">erlwitness:entity()</a>, EntityProcType::<a href="erlwitness.md#type-process_type">erlwitness:process_type()</a>, EntityPid::pid()) -&gt; ok
</code></pre>

<br></br>



<a name="start_link-1"></a>

### start_link/1 ###


<pre><code>
start_link(Id::pos_integer()) -&gt; {ok, Pid} | ignore | {error, Error}
</code></pre>

<br></br>



Starts the server

