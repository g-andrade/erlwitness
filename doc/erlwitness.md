

# Module erlwitness #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

## Data Types ##




### <a name="type-entity">entity()</a> ###



<pre><code>
entity() = any()
</code></pre>





### <a name="type-init_result">init_result()</a> ###



<pre><code>
init_result() = {ok, S::term()} | {ok, S::term(), timeout()} | ignore | {stop, Reason::term()}
</code></pre>





### <a name="type-process_type">process_type()</a> ###



<pre><code>
process_type() = any()
</code></pre>





### <a name="type-wrapped_init_args">wrapped_init_args()</a> ###


__abstract datatype__: `wrapped_init_args()`

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#finalize_init-2">finalize_init/2</a></td><td>Finalize entity's gen_server initialization.</td></tr><tr><td valign="top"><a href="#get_start_extras-2">get_start_extras/2</a></td><td>Wrap arguments and merge erlwitness custom debug options.</td></tr><tr><td valign="top"><a href="#get_start_extras-3">get_start_extras/3</a></td><td>Wrap arguments and merge erlwitness custom debug options.</td></tr><tr><td valign="top"><a href="#get_start_extras-4">get_start_extras/4</a></td><td>Wrap arguments and merge erlwitness custom debug options.</td></tr><tr><td valign="top"><a href="#unwrap_init_args-1">unwrap_init_args/1</a></td><td>Unwrap previously wrapped args.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="finalize_init-2"></a>

### finalize_init/2 ###


<pre><code>
finalize_init(Wrapped_init_args::#wrapped_init_args{}, InitResult::<a href="#type-init_result">init_result()</a>) -&gt; <a href="#type-init_result">init_result()</a>
</code></pre>

<br></br>


Finalize entity's gen_server initialization
<a name="get_start_extras-2"></a>

### get_start_extras/2 ###


<pre><code>
get_start_extras(Entity::<a href="#type-entity">entity()</a>, EntityProcType::<a href="#type-process_type">process_type()</a>) -&gt; {WrappedInitArgs::#wrapped_init_args{}, StartOptions::[term()]}
</code></pre>

<br></br>


Wrap arguments and merge erlwitness custom debug options.

<a name="get_start_extras-3"></a>

### get_start_extras/3 ###


<pre><code>
get_start_extras(Entity::<a href="#type-entity">entity()</a>, EntityProcType::<a href="#type-process_type">process_type()</a>, Args::term()) -&gt; {WrappedInitArgs::#wrapped_init_args{}, StartOptions::[term()]}
</code></pre>

<br></br>


Wrap arguments and merge erlwitness custom debug options.

<a name="get_start_extras-4"></a>

### get_start_extras/4 ###


<pre><code>
get_start_extras(Entity::<a href="#type-entity">entity()</a>, EntityProcType::<a href="#type-process_type">process_type()</a>, Args::term(), BaseStartOptions::[term()]) -&gt; {WrappedInitArgs::#wrapped_init_args{}, StartOptions::[term()]}
</code></pre>

<br></br>


Wrap arguments and merge erlwitness custom debug options.

<a name="unwrap_init_args-1"></a>

### unwrap_init_args/1 ###


<pre><code>
unwrap_init_args(WrappedInitArgs::#wrapped_init_args{}) -&gt; term()
</code></pre>

<br></br>


Unwrap previously wrapped args.
