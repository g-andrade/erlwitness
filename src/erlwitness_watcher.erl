% vim: set expandtab softtabstop=4 shiftwidth=4:
-module(erlwitness_watcher).
-author('Guilherme Andrade <erlwitness(at)gandrade(dot)net>').

-behaviour(gen_server).

%% API functions
-export([start_link/3,
         start_link/4,
         start/3,
         start/4,
         get_entity_dbg_options/3,
         install_dbg_fun/4,
         watch/1,
         unwatch/1,
         unwatch_all/0,
         report_init/7]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export_type([dbg_fun/0, dbg_fun_state/0, handler_return/0]).

-ignore_xref([{behaviour_info, 1},
              {start, 3},
              {start, 4},
              {start_link, 3},
              {start_link, 4},
              {install_dbg_fun, 4},
              {watch, 1},
              {unwatch, 1},
              {unwatch_all, 0}]).


-type handler_return() ::
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.

-callback handle_gencall_event(Timestamp :: erlang:timestamp(),
                               Entity :: erlwitness:entity(),
                               EntityPid :: pid(),
                               EntityProcType :: erlwitness:process_type(),
                               EntityProcName :: term(),
                               Call :: term(),
                               From :: {pid(), reference()},
                               State :: term()) -> handler_return().

-callback handle_gencast_event(Timestamp :: erlang:timestamp(),
                               Entity :: erlwitness:entity(),
                               EntityPid :: pid(),
                               EntityProcType :: erlwitness:process_type(),
                               EntityProcName :: term(),
                               Cast :: term(),
                               State :: term()) -> handler_return().

-callback handle_geninfo_event(Timestamp :: erlang:timestamp(),
                               Entity :: erlwitness:entity(),
                               EntityPid :: pid(),
                               EntityProcType :: erlwitness:process_type(),
                               EntityProcName :: term(),
                               Info :: term(),
                               State :: term()) -> handler_return().

-callback handle_newstate_event(Timestamp :: erlang:timestamp(),
                                Entity :: erlwitness:entity(),
                                EntityPid :: pid(),
                                EntityProcType :: erlwitness:process_type(),
                                EntityProcName :: term(),
                                EntityProcState :: term(),
                                State :: term()) -> handler_return().


-define(PROCDIC_WATCHER_MODULE, 'erlwitness/watcher_module').
-define(PROCDIC_ENTITY_STATE(Pid), {'erlwitness/entity_state', Pid}).


-define(WITNESSED_EVENT(Timestamp, Entity, EntityPid, EntityProcType, EntityProcName, Event),
        {'WITNESS', Timestamp, {Entity, EntityPid, EntityProcType, EntityProcName}, Event}).

-type dbg_fun_state() :: active | done.
-type dbg_fun() :: fun ((FuncState :: dbg_fun_state(), Event :: any(), ProcName :: any())
                        -> NewFuncState :: dbg_fun_state()).

%%%===================================================================
%%% API functions
%%%===================================================================
-spec get_entity_dbg_options(Entity :: erlwitness:entity(), EntityProcType :: erlwitness:process_type(),
                             Watchers :: [pid()])
    -> [{install, {dbg_fun(), dbg_fun_state()}}].
get_entity_dbg_options(Entity, EntityProcType, Watchers) ->
    [{install, dbg_fun(Entity, EntityProcType, Watcher)} || Watcher <- Watchers].


-spec install_dbg_fun(Entity :: erlwitness:entity(),
                      EntityProcType :: erlwitness:process_type(),
                      EntityPid :: pid(),
                      Watcher :: pid()) -> any().
install_dbg_fun(Entity, EntityProcType, EntityPid, Watcher) ->
    Dbgfun = dbg_fun(Entity, EntityProcType, Watcher),
    catch sys:install(EntityPid, Dbgfun, 20).


-spec watch(Entity :: erlwitness:entity()) -> ok.
watch(Entity) ->
    Watcher = self(),

    % Get ready for new procs
    true = erlwitness_lobby:watch(Entity, Watcher),

    % Inject into existing procs
    IndexedEntityRefs = lookup_global_entity(Entity),
    ok = lists:foreach(
            fun ({EntityProcType, EntityPid}) ->
                    catch rpc:cast(node(EntityPid), ?MODULE, install_dbg_fun,
                                   [Entity, EntityProcType, EntityPid, Watcher])
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

report_init(Watcher, Timestamp, Entity, EntityPid, EntityProcType, EntityProcName, EntityProcState) ->
    ok = gen_server:cast(Watcher, ?WITNESSED_EVENT(Timestamp, Entity, EntityPid, EntityProcType,
                                                   EntityProcName, {init, EntityProcState})).


%%%  -----------------------------------------------------------------
%%% Starts an watcher server.
%%% start(Entity, WatcherMod, WatcherArgs)
%%% start(Name, Entity, WatcherMod, WatcherArgs)
%%% start_link(Entity, WatcherMod, WatcherArgs)
%%% start_link(Name, Entity, WatcherMod, WatcherArgs) where:
%%%    Name ::= {local, atom()} | {global, atom()} | {via, atom(), term()}
%%%    WatcherMod  ::= atom(), callback module implementing the 'real' watcher server
%%%    WatcherArgs ::= term(), init arguments (to Mod:init/1)
%%% Returns: {ok, Pid} |
%%%          {error, {already_started, Pid}} |
%%%          {error, Reason}
%%% -----------------------------------------------------------------
start_link(Entities, WatcherModule, WatcherArgs) when is_list(Entities) ->
    gen_server:start_link(?MODULE, [Entities, WatcherModule, WatcherArgs], []).

start_link(Name, Entities, WatcherModule, WatcherArgs) when is_list(Entities) ->
    gen_server:start_link(Name, ?MODULE, [Entities, WatcherModule, WatcherArgs], []).

start(Entities, WatcherModule, WatcherArgs) when is_list(Entities) ->
    gen_server:start(?MODULE, [Entities, WatcherModule, WatcherArgs], []).

start(Name, Entities, WatcherModule, WatcherArgs) when is_list(Entities) ->
    gen_server:start(Name, ?MODULE, [Entities, WatcherModule, WatcherArgs], []).

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
init([Entities, WatcherModule, WatcherArgs]) ->
    undefined = put(?PROCDIC_WATCHER_MODULE, WatcherModule),
    gen_server:cast(self(), {watch, Entities}),
    WatcherModule:init(WatcherArgs).

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
handle_cast({watch, Entities}, State) ->
    lists:foreach(fun (Entity) -> ok = watch(Entity) end, Entities),
    {noreply, State};

handle_cast(?WITNESSED_EVENT(Timestamp, Entity, EntityPid, EntityProcType, EntityProcName, Event), State) ->
    WatcherModule = get(?PROCDIC_WATCHER_MODULE),
    case Event of
        {dbg, {in, {'$gen_call', From, Call}}} ->
            WatcherModule:handle_gencall_event(Timestamp, Entity, EntityPid, EntityProcType, EntityProcName,
                                               Call, From, State);
        {dbg, {in, {'$gen_cast', Cast}}} ->
            WatcherModule:handle_gencast_event(Timestamp, Entity, EntityPid, EntityProcType, EntityProcName,
                                               Cast, State);
        {dbg, {in, Info}} ->
            WatcherModule:handle_geninfo_event(Timestamp, Entity, EntityPid, EntityProcType, EntityProcName,
                                               Info, State);
        {dbg, {noreply, EntityProcState}} ->
            case get(?PROCDIC_ENTITY_STATE(EntityPid)) =:= EntityProcState of
                true  -> {noreply, State};
                false ->
                    put(?PROCDIC_ENTITY_STATE(EntityPid), EntityProcState),
                    WatcherModule:handle_newstate_event(Timestamp, Entity, EntityPid, EntityProcType,
                                                        EntityProcName, EntityProcState, State)
            end;
        {dbg, {out, _Reply, _To, EntityProcState}} ->
            case get(?PROCDIC_ENTITY_STATE(EntityPid)) =:= EntityProcState of
                true  -> {noreply, State};
                false ->
                    put(?PROCDIC_ENTITY_STATE(EntityPid), EntityProcState),
                    WatcherModule:handle_newstate_event(Timestamp, Entity, EntityPid, EntityProcType,
                                                        EntityProcName, EntityProcState, State)
            end;
        {init, EntityProcState} ->
            undefined = put(?PROCDIC_ENTITY_STATE(EntityPid), EntityProcState),
            WatcherModule:handle_newstate_event(Timestamp, Entity, EntityPid, EntityProcType,
                                                EntityProcName, EntityProcState, State)
    end;

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

report_dbg_event(Watcher, Timestamp, Entity, EntityPid, EntityProcType, EntityProcName, Event) ->
    ok = gen_server:cast(Watcher,  ?WITNESSED_EVENT(Timestamp, Entity, EntityPid, EntityProcType,
                                                    EntityProcName, {dbg, Event})).

-spec dbg_fun(Entity :: erlwitness:entity(), EntityProcType :: erlwitness:process_type(), Watcher :: pid())
    -> {dbg_fun(), FuncState :: active}.
dbg_fun(Entity, EntityProcType, Watcher) ->
    Fun = fun
        (active, Event, EntityProcName) ->
            case erlwitness_lobby:is_entity_watched_by(Entity, Watcher) of
                false -> done;
                true  ->
                    Timestamp = os:timestamp(),
                    ok = report_dbg_event(Watcher, Timestamp, Entity, self(),
                                          EntityProcType, EntityProcName,
                                          Event),
                    active
            end
    end,
    {Fun, active}.
