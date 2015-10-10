-module(erlwitness_gen_serv).
-behaviour(gen_server).

%% API functions
-export([start_link/5,
         start_link/6,
         start/5,
         start/6]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-define(PROCDIC_SERVER_MODULE, 'erlwitness/server_module').


%% Taken from gen_server.erl
-callback init(Args :: term()) ->
    {ok, State :: term()} | {ok, State :: term(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
-callback handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                      State :: term()) ->
    {reply, Reply :: term(), NewState :: term()} |
    {reply, Reply :: term(), NewState :: term(), timeout() | hibernate} |
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()}.
-callback handle_cast(Request :: term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.
-callback handle_info(Info :: timeout | term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.
-callback terminate(Reason :: (normal | shutdown | {shutdown, term()} |
                               term()),
                    State :: term()) ->
    term().
-callback code_change(OldVsn :: (term() | {down, term()}), State :: term(),
                      Extra :: term()) ->
    {ok, NewState :: term()} | {error, Reason :: term()}.


%%%===================================================================
%%% API functions
%%%===================================================================

%% Based on gen_serv.erl

%%%  -----------------------------------------------------------------
%%% Starts a generic server.
%%% start(Entity, EntityProcType, Mod, Args, Options)
%%% start(Entity, EntityProcType, Name, Mod, Args, Options)
%%% start_link(Entity, EntityProcType, Mod, Args, Options)
%%% start_link(Entity, EntityProcType, Name, Mod, Args, Options) where:
%%%    Entity ::= term()
%%%    EntityProcType ::= term()
%%%    Name ::= {local, atom()} | {global, atom()} | {via, atom(), term()}
%%%    Mod  ::= atom(), callback module implementing the 'real' server
%%%    Args ::= term(), init arguments (to Mod:init/1)
%%%    Options ::= [{timeout, Timeout} | {debug, [Flag]}]
%%%      Flag ::= trace | log | {logfile, File} | statistics | debug
%%%          (debug == log && statistics)
%%% Returns: {ok, Pid} |
%%%          {error, {already_started, Pid}} |
%%%          {error, Reason}
%%% -----------------------------------------------------------------

start(Entity, EntityProcType, Mod, Args, Options) ->
    {MergedOptions, Watchers} = merge_dbg_options(Entity, EntityProcType, Options),
    gen_server:start(?MODULE, [Watchers, Entity, EntityProcType, none,
                               Mod, Args], MergedOptions).

start(Entity, EntityProcType, Name, Mod, Args, Options) ->
    {MergedOptions, Watchers} = merge_dbg_options(Entity, EntityProcType, Options),
    gen_server:start(Name, ?MODULE, [Watchers, Entity, EntityProcType, {value, Name},
                                     Mod, Args], MergedOptions).

start_link(Entity, EntityProcType, Mod, Args, Options) ->
    {MergedOptions, Watchers} = merge_dbg_options(Entity, EntityProcType, Options),
    gen_server:start_link(?MODULE, [Watchers, Entity, EntityProcType, none,
                                    Mod, Args], MergedOptions).

start_link(Entity, EntityProcType, Name, Mod, Args, Options) ->
    {MergedOptions, Watchers} = merge_dbg_options(Entity, EntityProcType, Options),
    gen_server:start_link(Name, ?MODULE, [Watchers, Entity, EntityProcType, {value, Name},
                                          Mod, Args], MergedOptions).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Watchers, Entity, EntityProcType, MaybeName, Mod, Args]) ->
    _ = erlwitness:index_entity(Entity, EntityProcType, self()),
    EntityProcName = case MaybeName of
        {value, V} -> V;
        none -> self()
    end,

    Res = Mod:init(Args),
    MaybeState = case Res of
        {ok, State0} -> {value, State0};
        {ok, State0, _Timeout} -> {value, State0};
        _ -> none
    end,

    undefined = put(?PROCDIC_SERVER_MODULE, Mod),
    case MaybeState of
        none ->
            Res;
        {value, _State} when length(Watchers) < 1 ->
            Res;
        {value, State} ->
            Timestamp = os:timestamp(),
            lists:foreach(
                fun (Watcher) ->
                        ok = erlwitness_watcher:report_init(Watcher, Timestamp, Entity, self(),
                                                            EntityProcType, EntityProcName, State)
                end,
                Watchers),
            Res
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(Request, From, State) ->
    (get(?PROCDIC_SERVER_MODULE)):handle_call(Request, From, State).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    (get(?PROCDIC_SERVER_MODULE)):handle_cast(Msg, State).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    (get(?PROCDIC_SERVER_MODULE)):handle_info(Info, State).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    (get(?PROCDIC_SERVER_MODULE)):terminate(Reason, State).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    (get(?PROCDIC_SERVER_MODULE)):code_change(OldVsn, State, Extra).

%%%===================================================================
%%% Internal functions
%%%===================================================================
merge_dbg_options(Entity, EntityProcType, Options) ->
    Watchers = erlwitness_lobby:watchers_local_lookup(Entity),
    ExtraDebugOptions = erlwitness_watcher:get_entity_dbg_options(Entity, EntityProcType, Watchers),
    {MaybeMerged, MergeStatus} = lists:mapfoldl(
        fun ({debug, DebugOptions}, not_merged) ->
                {{debug, DebugOptions ++ ExtraDebugOptions},
                 merged};
            (V, Acc) ->
                {V, Acc}
        end,
        not_merged, Options),
    Merged = case MergeStatus of
        merged     -> MaybeMerged;
        not_merged -> [{debug, ExtraDebugOptions} | MaybeMerged]
    end,
    {Merged, Watchers}.
