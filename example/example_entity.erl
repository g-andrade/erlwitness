% vim: set expandtab softtabstop=4 shiftwidth=4:
-module(erlwitness_test_entity).
-behaviour(gen_server).

-export([start_link/3,
         start/3]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-record(state, {
        files :: term(),
        lucky_number :: number()
}).


start_link(Person, Files, LuckyNumber) ->
    {WrappedArgs, StartOptions} = erlwitness:get_start_extras(Person, person_file_serv,
                                                              [Files, LuckyNumber]),
    gen_server:start_link(?MODULE, WrappedArgs, StartOptions).

start(Person, Files, LuckyNumber) ->
    {WrappedArgs, StartOptions} = erlwitness:get_start_extras(Person, person_file_serv,
                                                              [Files, LuckyNumber]),
    gen_server:start(?MODULE, WrappedArgs, StartOptions).


init(WrappedArgs) ->
    [Files, LuckyNumber] = erlwitness:unwrap_init_args(WrappedArgs),
    InitResult = {ok, #state{files = Files,
                             lucky_number = LuckyNumber}},
    erlwitness:finalize_init(WrappedArgs, InitResult).

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


