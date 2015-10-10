% vim: set expandtab softtabstop=4 shiftwidth=4:
-module(erlwitness_lobby).
-behaviour(gen_server).

%% API functions
-export([watch/2,
         unwatch/2,
         unwatch_by_pid/1,
         lookup_local_watchers/1,
         is_entity_watched_by/2]).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-ignore_xref([{start_link, 0}]).


-record(state, {
        monitored_watchers = [] :: [{Monitor :: reference(), WatcherPid :: pid()}]
}).

-record(watcher, {
        entity :: erlwitness:entity(),
        watcher_pid :: pid()
}).


-define(TABLE, ?MODULE).
-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
-spec watch(Entity :: erlwitness:entity(), WatcherPid :: pid()) -> boolean().
watch(Entity, WatcherPid) ->
    Watcher = #watcher{entity = Entity,
                         watcher_pid = WatcherPid},
    {Replies, _BadNodes} = gen_server:multi_call(?SERVER, {watch, Watcher}),
    length(Replies) > 0.

-spec unwatch(Entity :: erlwitness:entity(), WatcherPid :: pid()) -> ok.
unwatch(Entity, WatcherPid) ->
    Watcher = #watcher{entity = Entity,
                         watcher_pid = WatcherPid},
    abcast = gen_server:abcast(?SERVER, {unwatch, Watcher}),
    ok.

-spec unwatch_by_pid(WatcherPid :: pid()) -> ok.
unwatch_by_pid(WatcherPid) ->
    abcast = gen_server:abcast(?SERVER, {unwatch_pid, WatcherPid}),
    ok.

-spec lookup_local_watchers(Entity :: erlwitness:entity()) -> [pid()].
lookup_local_watchers(Entity) ->
    Watchers = ets:lookup(?TABLE, Entity),
    [Watcher#watcher.watcher_pid || Watcher <- Watchers].

-spec is_entity_watched_by(Entity :: erlwitness:entity(), WatcherPid :: pid()) -> boolean().
is_entity_watched_by(Entity, WatcherPid) ->
    ets:member(?TABLE, #watcher{entity = Entity, watcher_pid=WatcherPid}).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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
init([]) ->
    ?TABLE = ets:new(?TABLE, [bag, named_table, protected,
                              {keypos, #watcher.entity},
                              {read_concurrency, true}]),
    {ok, #state{}}.

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
handle_call({watch, #watcher{ entity=Entity, watcher_pid=Pid }=Watcher}, _From, State)
        when is_pid(Pid), Pid /= self()
->
    Monitors = State#state.monitored_watchers,
    case {lists:member(Watcher, ets:lookup(?TABLE, Entity)),
          lists:keymember(Pid, 2, Monitors)}
    of
        {true, true} ->
            {reply, ok, State};
        {false, true} ->
            % We're already monitoring this watcher
            true = ets:insert(?TABLE, Watcher),
            {reply, ok, State};
        {false, false} ->
            Monitor = monitor(process, Pid),
            NewMonitors = [{Monitor, Pid} | State#state.monitored_watchers],
            NewState = State#state{ monitored_watchers=NewMonitors },
            true = ets:insert(?TABLE, Watcher),
            {reply, ok, NewState}
    end;

handle_call(_Request, _From, State) ->
    {noreply, State}.

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
handle_cast({unwatch, #watcher{ watcher_pid=Pid }}=Watcher, State) when is_pid(Pid) ->
    true = ets:delete_object(?TABLE, Watcher),
    case ets:match(?TABLE, #watcher{ entity='$1', watcher_pid=Pid }) of
        [_|_] -> {noreply, State};
        [] ->
            % Last watcher for this pid unwatched; unmonitor process
            {noreply, handle_pid_unregistration(State, Pid)}
    end;

handle_cast({unwatch_pid, Pid}, State) when is_pid(Pid) ->
    {noreply, handle_pid_unregistration(State, Pid)};

handle_cast(_Msg, State) ->
    {noreply, State}.

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
handle_info({'DOWN', _Ref, process, Pid, _Reason}, #state{}=State) ->
    {noreply, handle_pid_unregistration(State, Pid)};

handle_info(_Info, State) ->
    {noreply, State}.

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
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec handle_pid_unregistration(#state{}, pid()) -> #state{}.
handle_pid_unregistration(#state{ monitored_watchers=Monitors }=State, Pid) ->
    NewMonitors = lists:keydelete(Pid, 2, Monitors),
    true = ets:match_delete(?TABLE, #watcher{ entity='_', watcher_pid=Pid }),
    State#state{ monitored_watchers=NewMonitors }.
