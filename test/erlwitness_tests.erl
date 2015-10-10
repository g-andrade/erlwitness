% vim: set expandtab softtabstop=4 shiftwidth=4:
-module(erlwitness_tests).

-include_lib("eunit/include/eunit.hrl").

erlwitness_test_() ->
    {foreach,
     fun() ->
                error_logger:tty(false),
                random:seed(erlang:now()),
                {ok, _} = application:ensure_all_started(erlwitness)
        end,
     fun(_) ->
                %case whereis(pooldude_test) of
                %    undefined -> ok;
                %    Pid -> pool_call(Pid, stop)
                %end,
                ok = application:stop(erlwitness),
                error_logger:tty(true)
        end,
     [{<<"Watcher before entity">>, fun watcher_before_entity/0},
      {<<"Entity before watcher">>, fun entity_before_watcher/0}
            ]
    }.

watcher_before_entity() ->
    Self = self(),
    Entity = random_entity(),
    {ok, _WatcherProc} = erlwitness_test_watcher:start_link(Entity, Self),
    timer:sleep(20), % Give it enough time

    {ok, EntityProc} = erlwitness_test_entity:start(Entity, barfoo),
    ?assertMatch({new_state, _}, recv_msg()),

    Cast = "a cast",
    gen_server:cast(EntityProc, Cast),
    ?assertEqual({cast, Cast}, recv_msg()),

    Call = <<"a call">>,
    ok = gen_server:call(EntityProc, Call),
    ?assertMatch({call, Call, {Self, _Ref}}, recv_msg()),

    ChangeStateCast = change_state,
    gen_server:cast(EntityProc, ChangeStateCast),
    ?assertEqual({cast, ChangeStateCast}, recv_msg()),
    ?assertMatch({new_state, _}, recv_msg()),

    Info = 'an info',
    EntityProc ! Info,
    ?assertEqual({info, Info}, recv_msg()),

    StopCall = die,
    _StopCallReply = gen_server:call(EntityProc, StopCall),
    ?assertMatch({call, StopCall, {Self, _Ref}}, recv_msg()),
    ok.


entity_before_watcher() ->
    Self = self(),
    Entity = random_entity(),
    {ok, EntityProc} = erlwitness_test_entity:start(Entity, barfoo),
    timer:sleep(20), % Give it enough time

    {ok, _WatcherProc} = erlwitness_test_watcher:start_link(Entity, Self),
    timer:sleep(10),

    Cast = "a cast",
    gen_server:cast(EntityProc, Cast),
    ?assertEqual({cast, Cast}, recv_msg()),
    ?assertMatch({new_state, _}, recv_msg()),

    Call = <<"a call">>,
    ok = gen_server:call(EntityProc, Call),
    ?assertMatch({call, Call, {Self, _Ref}}, recv_msg()),

    ChangeStateCast = change_state,
    gen_server:cast(EntityProc, ChangeStateCast),
    ?assertEqual({cast, ChangeStateCast}, recv_msg()),
    ?assertMatch({new_state, _}, recv_msg()),

    Info = 'an info',
    EntityProc ! Info,
    ?assertEqual({info, Info}, recv_msg()),

    StopCall = die,
    _StopCallReply = gen_server:call(EntityProc, StopCall),
    ?assertMatch({call, StopCall, {Self, _Ref}}, recv_msg()),
    ok.

random_entity() ->
    {foobar, random:uniform(1 bsl 25)}.

recv_msg() ->
    receive V -> V end.
