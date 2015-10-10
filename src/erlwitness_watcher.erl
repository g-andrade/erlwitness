% vim: set expandtab softtabstop=4 shiftwidth=4:
-module(erlwitness_watcher).
-behaviour(gen_server).

%% API functions
-export([start_link/3,
         start/3,
         get_entity_dbg_options/2,
         watch/1,
         unwatch/1,
         unwatch_all/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-ignore_xref([{behaviour_info, 1},
              {start, 3},
              {start_link, 3},
              {watch, 1},
              {unwatch, 1},
              {unwatch_all, 0}]).


-callback handle_event(Timestamp :: erlang:timestamp(),
                       Entity :: erlwitness:entity(),
                       EntityPid :: pid(),
                       EntityProcType :: erlwitness:process_type(),
                       EntityProcState :: term(),
                       Event :: term(),
                       State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.


-define(PROCDIC_WATCHER_MODULE, 'erlwitness/watcher_module').
-define(WITNESSED_EVENT(Timestamp, Entity, EntityPid, EntityProcType, EntityProcState, Event),
        {'WITNESS', Timestamp, {Entity, EntityPid, EntityProcType, EntityProcState}, Event}).

-type dbg_fun(FuncStateT) :: fun ((FuncState :: FuncStateT, Event :: any(), State :: any())
                                  -> NewFuncState :: FuncStateT).

%%%===================================================================
%%% API functions
%%%===================================================================
-spec get_entity_dbg_options(Entity :: erlwitness:entity(), EntityProcType :: erlwitness:process_type()) -> [sys:dbg_opt()].
get_entity_dbg_options(Entity, EntityProcType) ->
    Watchers = erlwitness_lobby:watchers_local_lookup(Entity),
    [dbg_fun(Entity, EntityProcType, Watcher) || Watcher <- Watchers].

-spec watch(Entity :: erlwitness:entity()) -> ok.
watch(Entity) ->
    Watcher = self(),

    % Get ready for new procs
    true = erlwitness_lobby:watch(Entity, Watcher),

    % Inject into existing procs
    IndexedEntityRefs = lookup_global_entity(Entity),
    ok = lists:foreach(
            fun ({EntityProcType, Pid}) ->
                    Dbgfun = dbg_fun(Entity, EntityProcType, Watcher),
                    catch sys:install(Pid, Dbgfun, 20)
            end,
            IndexedEntityRefs).

-spec unwatch(Entity :: erlwitness:entity()) -> ok.
unwatch(Entity) ->
    Watcher = self(),
    ok = erlwitness_lobby:unwatch(Entity, Watcher).

-spec unwatch_all() -> ok.
unwatch_all() ->
    Watcher = self(),
    ok = erlwitness_lobby:unwatch_by_pid(Watcher).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Entity, WatcherModule, WatcherArgs) ->
    gen_server:start_link(?MODULE, [Entity, WatcherModule, WatcherArgs], []).

start(Entity, WatcherModule, WatcherArgs) ->
    gen_server:start(?MODULE, [Entity, WatcherModule, WatcherArgs], []).

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
init([Entity, WatcherModule, WatcherArgs]) ->
    undefined = put(?PROCDIC_WATCHER_MODULE, WatcherModule),
    gen_server:cast(self(), {watch, Entity}),
    apply(WatcherModule, init, WatcherArgs).

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
    (get(?PROCDIC_WATCHER_MODULE)):handle_call(Request, From, State).

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
handle_cast({watch, Entity}, State) ->
    ok = watch(Entity),
    {noreply, State};

handle_cast(?WITNESSED_EVENT(EntityPid, Timestamp, Event, EntityProcState, Entity, EntityProcType), State) ->
    WatcherModule = get(?PROCDIC_WATCHER_MODULE),
    WatcherModule:handle_event(EntityPid, Timestamp, Event, EntityProcState, Entity, EntityProcType, State);

handle_cast(Msg, State) ->
    (get(?PROCDIC_WATCHER_MODULE)):handle_cast(Msg, State).

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
    (get(?PROCDIC_WATCHER_MODULE)):handle_info(Info, State).

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
    ok = unwatch_all(),
    (get(?PROCDIC_WATCHER_MODULE)):terminate(Reason, State).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    (get(?PROCDIC_WATCHER_MODULE)):code_change(OldVsn, State, Extra).

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec lookup_global_entity(Entity :: erlwitness:entity()) -> [erlwitness_lookup:indexed_entity_ref()].
lookup_global_entity(Entity) ->
    Module = erlwitness_conf:get_lookup_module(),
    Module:lookup_global_entity(Entity).

report_event(Watcher, Timestamp, Entity, EntityPid, EntityProcType, EntityProcState, Event) ->
    ok = gen_server:cast(Watcher,  ?WITNESSED_EVENT(Timestamp, Entity, EntityPid, EntityProcType,
                                                    EntityProcState, Event)).

-spec dbg_fun(Entity :: erlwitness:entity(), EntityProcType :: erlwitness:process_type(), Watcher :: pid())
        -> {dbg_fun(active | idle), FuncState :: active}.
dbg_fun(Entity, EntityProcType, Watcher) ->
    Fun = fun
        (_PrevState, Event, EntityProcState) ->
            case erlwitness_lobby:is_entity_watched_by(Entity, Watcher) of
                false -> idle;
                true  ->
                    Timestamp = os:timestamp(),
                    ok = report_event(Watcher, Timestamp, Entity, self(),
                                      EntityProcType, EntityProcState,
                                      Event),
                    active
            end
    end,
    {Fun, active}.
