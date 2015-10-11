

# Module erlwitness_entity #
* [Function Index](#index)
* [Function Details](#functions)


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_entity-0">get_entity/0</a></td><td></td></tr><tr><td valign="top"><a href="#report_lager_event-8">report_lager_event/8</a></td><td></td></tr><tr><td valign="top"><a href="#set_params-3">set_params/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get_entity-0"></a>

### get_entity/0 ###


<pre><code>
get_entity() -&gt; {value, <a href="erlwitness.md#type-entity">erlwitness:entity()</a>} | undefined
</code></pre>

<br></br>



<a name="report_lager_event-8"></a>

### report_lager_event/8 ###


<pre><code>
report_lager_event(Watchers::[pid()], Entity::<a href="erlwitness.md#type-entity">erlwitness:entity()</a>, LagerModule::module(), LagerFunction::atom(), LagerArgs::list(), CodeModule::module(), CodeFunction::atom(), CodeLine::pos_integer()) -&gt; ok
</code></pre>

<br></br>



<a name="set_params-3"></a>

### set_params/3 ###


<pre><code>
set_params(Entity::<a href="erlwitness.md#type-entity">erlwitness:entity()</a>, EntityProcType::<a href="erlwitness.md#type-process_type">erlwitness:process_type()</a>, EntityProcName::term()) -&gt; ok
</code></pre>

<br></br>



