% vim: set expandtab softtabstop=4 shiftwidth=4:
-module(erlwitness_index_serv).

-behaviour(gen_server).
-behaviour(erlwitness_lookup).

%% API functions
-export([child_spec/1,
         register_entity/3,
         lookup_entity/1,
         start_link/1]).

%% erlwiness_lookup callbacks
-export([lookup_global_entity/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-ignore_xref([{start_link, 1},
              {lookup_entity, 1}]).


-record(state, {
        table = ets:tid()
}).

-record(indexed_entity, {
        entity :: erlwitness:entity(),
        proc_type :: erlwitness:process_type() | '$1',
        pid :: pid() | '$2',
        monitor :: reference() | '_'
}).


-define(BASE_TABLE, ?MODULE).
-define(BASE_SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
-spec child_spec(Id :: pos_integer()) -> supervisor:child_spec().
child_spec(Id) ->
    {server_for_id(Id), {?MODULE, start_link, [Id]}, permanent, 5000, worker, [?MODULE]}.

-spec register_entity(Entity :: erlwitness:entity(), EntityProcType :: erlwitness:process_type(),
                      EntityPid :: pid()) -> ok.
register_entity(Entity, EntityProcType, EntityPid) when is_pid(EntityPid), node(EntityPid) == node() ->
    Id = pick_id(Entity, EntityProcType, EntityPid),
    ok = gen_server:call(server_for_id(Id), {index, Entity, EntityProcType, EntityPid}, infinity).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Id) ->
    gen_server:start_link({local, server_for_id(Id)}, ?MODULE, [Id], []).


%%%===================================================================
%%% erlwitness_lookup callbacks
%%%===================================================================

-spec lookup_global_entity(Entity :: erlwitness:entity()) -> [erlwitness_lookup:indexed_entity_ref()].
lookup_global_entity(Entity) ->
    {Results, _BadNodes} = rpc:multicall(?MODULE, lookup_entity, [Entity]),
    sets:to_list( sets:from_list( lists:foldl(fun erlang:'++'/2, [], Results) ) ).


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
init([Id]) ->
    Table = table_for_id(Id),
    Table = ets:new(Table, [set, named_table, protected,
                            {keypos, #indexed_entity.monitor}]),
    {ok, #state{ table=Table }}.

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
handle_call({register, Entity, EntityProcType, Pid}, _From, #state{}=State) ->
    IndexedEntity = #indexed_entity{entity = Entity,
                                    proc_type = EntityProcType,
                                    pid = Pid,
                                    monitor = monitor(process, Pid)},
    true = ets:insert_new(State#state.table, IndexedEntity),
    {noreply, State};

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
handle_info({'DOWN', Ref, process, _Pid, _Reason}, #state{}=State) ->
    true = ets:delete(State#state.table, Ref),
    {noreply, State};

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
-spec index_serv_count() -> pos_integer().
index_serv_count() ->
    erlwitness_conf:get_index_serv_count().

-spec server_for_id(Id :: pos_integer()) -> atom().
server_for_id(Id) ->
    list_to_atom(atom_to_list(?BASE_SERVER) ++ "_" ++ integer_to_list(Id)).

-spec table_for_id(Id :: pos_integer()) -> atom().
table_for_id(Id) ->
    list_to_atom(atom_to_list(?BASE_TABLE) ++ "_" ++ integer_to_list(Id)).

-spec pick_id(Entity :: erlwitness:entity(), EntityProcType :: erlwitness:process_type(),
              EntityPid :: pid()) -> pos_integer().
pick_id(Entity, EntityProcType, EntityPid) ->
    Total = index_serv_count(),
    1 + erlang:phash2({Entity, EntityProcType, EntityPid, os:timestamp()}, Total).

-spec lookup_entity(Entity :: erlwitness:entity()) -> [erlwitness_lookup:indexed_entity_ref()].
lookup_entity(Entity) ->
    Match = #indexed_entity{entity = Entity,
                            proc_type = '$1',
                            pid = '$2',
                            monitor = '_'},
    MatchesById = [match_over_id(Id, Match) || Id <- lists:seq(1, index_serv_count())],
    Matches = lists:foldl(fun erlang:'++'/2, [], MatchesById),
    [{EntityProcType, Pid} || [EntityProcType, Pid] <- Matches].

-spec match_over_id(Id :: pos_integer(),
                    Match :: #indexed_entity{proc_type :: '$1', pid :: '$2', monitor :: '_'})
        -> [[any()]].
match_over_id(Id, Match) ->
    ets:match(table_for_id(Id), Match).
