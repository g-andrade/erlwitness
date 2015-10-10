% vim: set expandtab softtabstop=4 shiftwidth=4:
-module(erlwitness).
-export([get_starting_extras/2,
         get_starting_extras/3,
         get_starting_extras/4,
         unwrap_init_args/1,
         finalize_init/2
         %index_entity/3
        ]).
-export_type([entity/0, process_type/0, wrapped_init_args/0]).

-ignore_xref([{get_entity_dbg_options, 2}, {index_entity, 3}]).

-type entity() :: any().
-type process_type() :: any().
-type init_result() :: {ok, S::term()} | {ok, S::term(), timeout()} | ignore | {stop, Reason::term()}.

-record(wrapped_init_args, {
        watchers :: [pid()],
        entity :: entity(),
        entity_proc_type :: process_type(),
        args :: term()
}).

-opaque wrapped_init_args() :: #wrapped_init_args{}.


-spec get_starting_extras(Entity :: entity(), EntityProcType :: process_type())
    -> {WrappedInitArgs :: #wrapped_init_args{}, StartOptions :: [term()]}.
get_starting_extras(Entity, EntityProcType) ->
    get_starting_extras(Entity, EntityProcType, []).

-spec get_starting_extras(Entity :: entity(), EntityProcType :: process_type(),
                          Args :: term())
    -> {WrappedInitArgs :: #wrapped_init_args{}, StartOptions :: [term()]}.
get_starting_extras(Entity, EntityProcType, Args) ->
    get_starting_extras(Entity, EntityProcType, Args, []).

-spec get_starting_extras(Entity :: entity(), EntityProcType :: process_type(),
                          Args :: term(), BaseStartOptions :: [term()])
    -> {WrappedInitArgs :: #wrapped_init_args{}, StartOptions :: [term()]}.
    %DebugOptions :: [{install, {erlwitness_watcher:dbg_fun(), erlwitness_watcher:dbg_fun_state()}}]}.
get_starting_extras(Entity, EntityProcType, Args, BaseStartOptions) ->
    {StartOptions, Watchers} = merge_dbg_options(Entity, EntityProcType, BaseStartOptions),
    WrappedInitArgs = #wrapped_init_args{watchers = Watchers,
                                         entity = Entity,
                                         entity_proc_type = EntityProcType,
                                         args = Args},
    {WrappedInitArgs, StartOptions}.

-spec unwrap_init_args(WrappedInitArgs :: #wrapped_init_args{}) -> term().
unwrap_init_args(#wrapped_init_args{ args=Args }) -> Args.

-spec finalize_init(#wrapped_init_args{}, InitResult :: init_result()) -> init_result().
finalize_init(#wrapped_init_args{}, ignore) ->
    ignore;
finalize_init(#wrapped_init_args{}, {stop, Reason}) ->
    {stop, Reason};
finalize_init(#wrapped_init_args{}=WrappedInitArgs, InitResult) ->
    State0 = case InitResult of
        {ok, State} -> State;
        {ok, State, _Timeout} -> State
    end,

    #wrapped_init_args{ entity=Entity,
                       entity_proc_type=EntityProcType }=WrappedInitArgs,
    _ = index_entity(Entity, EntityProcType, self()),

    case WrappedInitArgs#wrapped_init_args.watchers of
        [] -> InitResult;
        [_|_]=Watchers ->
            Timestamp = os:timestamp(),
            lists:foreach(
                fun (Watcher) ->
                        ok = erlwitness_watcher:report_init(Watcher, Timestamp, Entity, self(),
                                                            EntityProcType, self(), State0)
                end,
                Watchers),
            InitResult
    end.


-spec index_entity(Entity :: entity(), EntityProcType :: process_type(), EntityPid :: pid())
        -> ok | {error, not_using_internal_indexing}.
index_entity(Entity, EntityProcType, EntityPid) ->
    case erlwitness_conf:use_internal_indexing() of
        false -> {error, not_using_internal_indexing};
        true ->
            ok = erlwitness_index_serv:register_entity(Entity, EntityProcType, EntityPid)
    end.

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
