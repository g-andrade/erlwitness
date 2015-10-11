% vim: set expandtab softtabstop=4 shiftwidth=4:
-module(erlwitness_tests).
-author('Guilherme Andrade <erlwitness(at)gandrade(dot)net>').

-include_lib("eunit/include/eunit.hrl").

erlwitness_test_() ->
    {foreach,
     fun() ->
                error_logger:tty(false),
                {ok, _} = application:ensure_all_started(erlwitness)
        end,
     fun(_) ->
                ok = application:stop(erlwitness),
                ok = application:stop(lager),
                ok = application:stop(goldrush),
                error_logger:tty(true)
        end,
     [{<<"Watcher before entity">>, fun watcher_before_entity/0},
      {<<"Entity before watcher">>, fun entity_before_watcher/0},
      {<<"Multiple entity watchers">>, fun multiple_entity_watchers/0},
      {<<"Multiple entity processes">>, fun multiple_entity_processes/0},
      {<<"Multiple everything">>, fun multiple_everything/0},
      {<<"Watcher death">>, fun watcher_death/0},
      {<<"Unwatching">>, fun unwatching/0}
            ]
    }.

watcher_before_entity() ->
    random:seed(erlang:now()),
    Self = self(),
    Entity = random_entity(),
    {ok, WatcherProc} = erlwitness_test_watcher:start_link([Entity], Self),
    timer:sleep(20), % Give it enough time

    EntityProcType = barfoo,
    {ok, EntityProc} = erlwitness_test_entity:start_link(Entity, EntityProcType),
    ?assertMatch({EntityProc, EntityProcType, {new_state, _}, WatcherProc}, recv_msg()),

    Cast = {log_message, "a cast"},
    gen_server:cast(EntityProc, Cast),
    ?assertEqual({EntityProc, EntityProcType, {cast, Cast}, WatcherProc}, recv_msg()),
    ?assertMatch({EntityProc, EntityProcType, {lager, {lager, debug, _},
                                               {erlwitness_test_entity, handle_cast, _}},
                  WatcherProc}, recv_msg()),

    Call = <<"a call">>,
    ok = gen_server:call(EntityProc, Call),
    ?assertMatch({EntityProc, EntityProcType, {call, Call, {Self, _Ref}}, WatcherProc}, recv_msg()),

    ChangeStateCast = change_state,
    gen_server:cast(EntityProc, ChangeStateCast),
    ?assertEqual({EntityProc, EntityProcType, {cast, ChangeStateCast}, WatcherProc}, recv_msg()),
    ?assertMatch({EntityProc, EntityProcType, {new_state, _}, WatcherProc}, recv_msg()),

    Info = 'an info',
    EntityProc ! Info,
    ?assertEqual({EntityProc, EntityProcType, {info, Info}, WatcherProc}, recv_msg()),

    StopCall = die,
    _StopCallReply = gen_server:call(EntityProc, StopCall),
    ?assertMatch({EntityProc, EntityProcType, {call, StopCall, {Self, _Ref}}, WatcherProc}, recv_msg()),
    ok.

entity_before_watcher() ->
    random:seed(erlang:now()),
    Self = self(),
    Entity = random_entity(),
    EntityProcType = foobar,
    {ok, EntityProc} = erlwitness_test_entity:start_link(Entity, EntityProcType),
    timer:sleep(20), % Give it enough time

    {ok, WatcherProc} = erlwitness_test_watcher:start_link([Entity], Self),
    timer:sleep(10),

    Cast = {log_message, "a cast"},
    ok = gen_server:cast(EntityProc, Cast),
    ?assertEqual({EntityProc, EntityProcType, {cast, Cast}, WatcherProc}, recv_msg()),
    ?assertMatch({EntityProc, EntityProcType, {lager, {lager, debug, _},
                                               {erlwitness_test_entity, handle_cast, _}},
                  WatcherProc}, recv_msg()),
    ?assertMatch({EntityProc, EntityProcType, {new_state, _}, WatcherProc}, recv_msg()),

    Call = <<"a call">>,
    ok = gen_server:call(EntityProc, Call),
    ?assertMatch({EntityProc, EntityProcType, {call, Call, {Self, _Ref}}, WatcherProc}, recv_msg()),

    ChangeStateCast = change_state,
    ok = gen_server:cast(EntityProc, ChangeStateCast),
    ?assertEqual({EntityProc, EntityProcType, {cast, ChangeStateCast}, WatcherProc}, recv_msg()),
    ?assertMatch({EntityProc, EntityProcType, {new_state, _}, WatcherProc}, recv_msg()),

    Info = 'an info',
    EntityProc ! Info,
    ?assertEqual({EntityProc, EntityProcType, {info, Info}, WatcherProc}, recv_msg()),

    StopCall = die,
    _StopCallReply = gen_server:call(EntityProc, StopCall),
    ?assertMatch({EntityProc, EntityProcType, {call, StopCall, {Self, _Ref}}, WatcherProc}, recv_msg()),
    ok.

multiple_entity_watchers() ->
    random:seed(erlang:now()),
    Self = self(),
    Entity = random_entity(),
    EntityProcType = {dunno, random_blob()},

    WatcherCount = 5 + random:uniform(200),
    Watchers = [
            begin
                {ok, WatcherProc} = erlwitness_test_watcher:start_link([Entity], Self),
                WatcherProc
            end
            || _ <- lists:seq(1, WatcherCount)],
    timer:sleep(30),

    {ok, EntityProc} = erlwitness_test_entity:start_link(Entity, EntityProcType),
    timer:sleep(10),

    MessageBatch1 = lists:sort( recv_msgs(WatcherCount) ),
    lists:foreach(
        fun ({WatcherProc, {MBEntityProc, MBEntityProcType, MBEvent, MBWatcherProc}}) ->
                ?assertMatch({EntityProc, EntityProcType, {new_state, _}, WatcherProc},
                             {MBEntityProc, MBEntityProcType, MBEvent, MBWatcherProc})
        end,
        lists:zip(Watchers, MessageBatch1)),

    TestInfo = {woohoo, random_blob()},
    EntityProc ! TestInfo,
    MessageBatch2 = lists:sort( recv_msgs(WatcherCount) ),
    lists:foreach(
        fun ({WatcherProc, {MBEntityProc, MBEntityProcType, MBEvent, MBWatcherProc}}) ->
                ?assertMatch({EntityProc, EntityProcType, {info, _}, WatcherProc},
                             {MBEntityProc, MBEntityProcType, MBEvent, MBWatcherProc})
        end,
        lists:zip(Watchers, MessageBatch2)).



multiple_entity_processes() ->
    random:seed(erlang:now()),
    Self = self(),
    Entity = random_entity(),
    {ok, WatcherProc} = erlwitness_test_watcher:start_link([Entity], Self),
    timer:sleep(10),

    ProcCount = 5 + random:uniform(200),
    BaseEntityProcRefs = [
            begin
                EntityProcType = random_blob(),
                {ok, EntityProc} = erlwitness_test_entity:start_link(Entity, EntityProcType),
                {EntityProc, EntityProcType}
            end
            || _ <- lists:seq(1, ProcCount)],
    EntityProcRefs = lists:sort(BaseEntityProcRefs),
    timer:sleep(30),


    MessageBatch1 = lists:sort( recv_msgs(ProcCount) ),
    lists:foreach(
        fun ({{RefEntityProc, RefEntityProcType}, {MBEntityProc, MBEntityProcType,
                                                   MBEvent, MBWatcherProc}}) ->
                ?assertMatch({RefEntityProc, RefEntityProcType, {new_state, _}, WatcherProc},
                             {MBEntityProc, MBEntityProcType, MBEvent, MBWatcherProc})
        end,
        lists:zip(EntityProcRefs, MessageBatch1)),


    lists:foreach(fun ({EntityProc, _EntityProcType}) -> ok = gen_server:cast(EntityProc, change_state) end,
                  EntityProcRefs),
    {BaseMessageBatch2, BaseMessageBatch3} = lists:split(ProcCount,
                                                         lists:keysort(3, recv_msgs(ProcCount * 2) )),
    MessageBatch2 = lists:sort(BaseMessageBatch2),
    MessageBatch3 = lists:sort(BaseMessageBatch3),
    lists:foreach(
        fun ({{RefEntityProc, RefEntityProcType}, {MBEntityProc, MBEntityProcType,
                                                   MBEvent, MBWatcherProc}}) ->
                ?assertEqual({RefEntityProc, RefEntityProcType, {cast, change_state}, WatcherProc},
                             {MBEntityProc, MBEntityProcType, MBEvent, MBWatcherProc})
        end,
        lists:zip(EntityProcRefs, MessageBatch2)),
    lists:foreach(
        fun ({{RefEntityProc, RefEntityProcType}, {MBEntityProc, MBEntityProcType,
                                                   MBEvent, MBWatcherProc}}) ->
                ?assertMatch({RefEntityProc, RefEntityProcType, {new_state, _}, WatcherProc},
                             {MBEntityProc, MBEntityProcType, MBEvent, MBWatcherProc})
        end,
        lists:zip(EntityProcRefs, MessageBatch3)).

multiple_everything() ->
    random:seed(erlang:now()),
    Self = self(),
    EntityCount = 2 + random:uniform(1000),
    Entities = [random_entity() || _ <- lists:seq(1, EntityCount)],

    WatcherCount = 5 + random:uniform(500),
    WatcherRefs = [
            begin
                Entity1 = pick_random_from_list(Entities),
                Entity2 = pick_random_from_list(Entities),
                Entity3 = pick_random_from_list(Entities),
                {ok, WatcherProc} = erlwitness_test_watcher:start_link([Entity1, Entity2, Entity3], Self),
                {WatcherProc, [Entity1, Entity2, Entity3]}
            end
            || _ <- lists:seq(1, WatcherCount)],
    timer:sleep(30),

    ProcCount = 5 + random:uniform(1200),
    BaseEntityProcRefs = [
            begin
                EntityProcType = random_blob(),
                Entity = pick_random_from_list(Entities),
                {ok, EntityProc} = erlwitness_test_entity:start_link(Entity, EntityProcType),
                {EntityProc, EntityProcType, Entity}
            end
            || _ <- lists:seq(1, ProcCount)],
    EntityProcRefs = lists:sort(BaseEntityProcRefs),
    timer:sleep(30),

    SpawnedEntities = [Entity || {_EntityProc, _EntityProcType, Entity} <- EntityProcRefs],
    SpawnedEntitiesSet = sets:from_list(SpawnedEntities),
    RelevantWatcherRefs = lists:filter(
            fun ({_WatcherProc, WatchedEntities}) ->
                    not sets:is_disjoint(sets:from_list(WatchedEntities), SpawnedEntitiesSet)
            end,
            WatcherRefs),
    ExpectedEventCount = lists:sum([sets:size(sets:intersection(SpawnedEntitiesSet,
                                                                sets:from_list(WatchedEntities)))
                                    || {_WatcherProc, WatchedEntities} <- RelevantWatcherRefs]),

    MessageBatch1 = recv_msgs(ExpectedEventCount),
    lists:foreach(
        fun ({MBEntityProc, MBEntityProcType, MBEvent, MBWatcherProc}) ->
                {WatcherProc, WatchedEntities} = lists:keyfind(MBWatcherProc, 1, RelevantWatcherRefs),
                {EntityProc, EntityProcType, Entity} = lists:keyfind(MBEntityProc, 1, EntityProcRefs),
                ?assert( lists:member(Entity, WatchedEntities) ),
                ?assertMatch({EntityProc, EntityProcType, {new_state, _}, WatcherProc},
                             {MBEntityProc, MBEntityProcType, MBEvent, MBWatcherProc})
        end,
        MessageBatch1).

watcher_death() ->
    random:seed(erlang:now()),
    Self = self(),
    Entity = random_entity(),

    WatcherCount = 10 + random:uniform(50),
    Watchers1 = lists:sort([
                begin
                    {ok, WatcherProc} = erlwitness_test_watcher:start([Entity], Self),
                    WatcherProc
                end
                || _ <- lists:seq(1, WatcherCount)]),
    timer:sleep(20),
    RegisteredWatchers1 = lists:sort( erlwitness_lobby:watchers_local_lookup(Entity) ),
    ?assertEqual(Watchers1, RegisteredWatchers1),
    {ok, EntityProc} = erlwitness_test_entity:start_link(Entity, random_blob()),
    timer:sleep(10),
    _MessageBatch1 = recv_msgs(length(RegisteredWatchers1)), % Events captured by all watchers
    ?assertEqual({message_queue_len, 0}, process_info(Self, message_queue_len)),

    {DeathRowA, Watchers2} = lists:mapfoldl(
            fun (_, Acc) ->
                    Watcher = pick_random_from_list(Acc),
                    {Watcher, lists:delete(Watcher, Acc)}
            end,
            Watchers1, lists:seq(1, length(RegisteredWatchers1) div 2)),
    [exit(Watcher, kill) || Watcher <- DeathRowA],
    timer:sleep(20),
    RegisteredWatchers2 = lists:sort( erlwitness_lobby:watchers_local_lookup(Entity) ),
    ?assertEqual(Watchers2, RegisteredWatchers2),
    ok = gen_server:cast(EntityProc, random_blob()),
    timer:sleep(5),
    _MessageBatch2 = recv_msgs(length(RegisteredWatchers2)), % Events captured only by remaining watchers
    ?assertEqual({message_queue_len, 0}, process_info(Self, message_queue_len)),

    DeathRowB = RegisteredWatchers2,
    [exit(Watcher, kill) || Watcher <- DeathRowB],
    timer:sleep(20),
    ?assertEqual([], erlwitness_lobby:watchers_local_lookup(Entity)),
    ok = gen_server:cast(EntityProc, random_blob()),
    ?assertEqual({message_queue_len, 0}, process_info(Self, message_queue_len)). % No watchers, no events.

unwatching() ->
    random:seed(erlang:now()),
    Self = self(),

    WatcherCount = 5 + random:uniform(20),
    WatcherRefs = lists:sort([
                begin
                    EntityCount = random:uniform(5),
                    Entities = [random_entity() || _ <- lists:seq(1, EntityCount)],
                    {ok, WatcherProc} = erlwitness_test_watcher:start(Entities, Self),
                    {WatcherProc, Entities}
                end
                || _ <- lists:seq(1, WatcherCount)]),
    timer:sleep(20),
    Watchers = [Watcher || {Watcher, _Entities} <- WatcherRefs],
    AllEntities = sets:to_list( sets:from_list( lists:foldl(fun erlang:'++'/2, [],
                                                            [Entities || {_Watcher, Entities}
                                                                         <- WatcherRefs]))),
    RegisteredWatchers1 = sets:to_list( sets:from_list(
                lists:foldl(fun erlang:'++'/2, [],
                            [erlwitness_lobby:watchers_local_lookup(Entity)
                             || Entity <- AllEntities]))),
    ?assertEqual(Watchers, lists:sort(RegisteredWatchers1)),

    {ToUnwatch, RemainingEntities} = lists:mapfoldl(
            fun (V, Acc) ->
                    {pick_random_from_list(Acc), lists:delete(V, Acc)}
            end,
            AllEntities, lists:seq(1, length(AllEntities) div 3)),
    lists:foreach(
        fun (Entity) ->
                EntityWatchers = erlwitness_lobby:watchers_local_lookup(Entity),
                lists:foreach(
                    fun (Watcher) ->
                            ok = gen_server:cast(Watcher, {unwatch, Entity})
                    end,
                    EntityWatchers),
                timer:sleep(20),
                ?assertEqual([], erlwitness_lobby:watchers_local_lookup(Entity))
        end,
        ToUnwatch),


    lists:foreach(fun (Watcher) -> ok = gen_server:cast(Watcher, unwatch_all) end,
                  Watchers),
    timer:sleep(20),
    RegisteredWatchers3 = sets:to_list( sets:from_list(
                lists:foldl(fun erlang:'++'/2, [],
                            [erlwitness_lobby:watchers_local_lookup(Entity)
                             || Entity <- RemainingEntities]))),
    ?assertEqual([], RegisteredWatchers3).

random_entity() ->
    {foobar, random_blob()}.

random_blob() ->
    random:uniform(1 bsl 25).

pick_random_from_list(L) ->
    lists:nth(random:uniform(length(L)), L).

recv_msgs(N) ->
    [recv_msg() || _ <- lists:seq(1, N)].

recv_msg() ->
    receive V -> V end.
