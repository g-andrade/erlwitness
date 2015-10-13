% vim: set expandtab softtabstop=4 shiftwidth=4:
-module(erlwitness_transform).
-export([parse_transform/2]).
-ignore_xref([{parse_transform, 2}]).

%X @doc Derived from basho/lager's lager_transform.erl (under Apache License 2.0)
%X

-include_lib("lager/include/lager.hrl").

-ifndef(LEVELS_UNSAFE).
-define(LEVELS_UNSAFE, []). % Not present in older lager versions
-endif.


%% @private
parse_transform(AST, _Options) ->
    walk_ast([], AST).

walk_ast(Acc, []) ->
    lists:reverse(Acc);
walk_ast(Acc, [{attribute, _, module, {Module, _PmodArgs}}=H|T]) ->
    put(module, Module),
    walk_ast([H|Acc], T);
walk_ast(Acc, [{attribute, _, module, Module}=H|T]) ->
    put(module, Module),
    walk_ast([H|Acc], T);
walk_ast(Acc, [{function, Line, Name, Arity, Clauses}|T]) ->
    put(function, Name),
    walk_ast([{function, Line, Name, Arity,
                walk_clauses([], Clauses)}|Acc], T);
walk_ast(Acc, [H|T]) ->
    walk_ast([H|Acc], T).

walk_clauses(Acc, []) ->
    lists:reverse(Acc);
walk_clauses(Acc, [{clause, Line, Arguments, Guards, Body}|T]) ->
    walk_clauses([{clause, Line, Arguments, Guards, walk_body([], Body)}|Acc], T).

walk_body(Acc, []) ->
    lists:reverse(Acc);
walk_body(Acc, [H|T]) ->
    walk_body([transform_statement(H)|Acc], T).

transform_statement({call, Line, {remote, _Line1, {atom, _Line2, lager=Module},
                                  {atom, _Line3, Function}}, Arguments0} = Stmt)
->
    case (lists:member(Function, ?LEVELS) orelse lists:keymember(Function, 1, ?LEVELS_UNSAFE)) of
        true ->
            blockify_stmts(Line, [do_transform(Line, Module, Function, Arguments0),
                                  Stmt]);
        false ->
            Stmt
    end;
transform_statement(Stmt) when is_tuple(Stmt) ->
    list_to_tuple(transform_statement(tuple_to_list(Stmt)));
transform_statement(Stmt) when is_list(Stmt) ->
    [transform_statement(S) || S <- Stmt];
transform_statement(Stmt) ->
    Stmt.

do_transform(Line, LagerModule, LagerFunction, LagerArguments) ->
    % Let's make sure we don't end up exporting any conflicting case variables
    EntityVar = list_to_atom("ErlWitness_LagerWrapper_SelfEntity_" ++ integer_to_list(Line)),
    WatchersVar = list_to_atom("ErlWitness_LagerWrapper_Watchers_" ++ integer_to_list(Line)),

    %% case erlwitness_entity:get_entity() of
    %%  undefined -> ok;
    %%  {value, Entity} ->
    %%      case erlwitness_lobby:watchers_local_lookup(Entity) of
    %%          [] -> ok;
    %%          [_|_]=Watchers ->
    %%              erlwitness_entity:report_lager_event(
    %%                  Watchers, Entity,
    %%                  $LAGER_MODULE, $LAGER_FUNCTION, $LAGER_ARGUMENTS$,
    %%                  $MODULE, $FUNCTION, $LINE)
    %%      end
    %% end

    {'case',Line,
     {call,Line,
      {remote,Line,{atom,Line,erlwitness_entity},{atom,Line,get_entity}},
      []},
     [{clause,Line,[{atom,Line,undefined}],[],[{atom,Line,ok}]},
      {clause,Line,
       [{tuple,Line,[{atom,Line,value},{var,Line,EntityVar}]}],
       [],
       [{'case',Line,
         {call,Line,
          {remote,Line,
           {atom,Line,erlwitness_lobby},
           {atom,Line,watchers_local_lookup}},
          [{var,Line,EntityVar}]},
         [{clause,Line,[{nil,Line}],[],[{atom,Line,ok}]},
          {clause,Line,
           [{match,Line,
             {cons,Line,{var,Line,'_'},{var,Line,'_'}},
             {var,Line,WatchersVar}}],
           [],
           [{call,Line,
             {remote,Line,
              {atom,Line,erlwitness_entity},
              {atom,Line,report_lager_event}},
             [{var,Line,WatchersVar},
              {var,Line,EntityVar},
              {atom,Line,LagerModule},
              {atom,Line,LagerFunction},
              tailify_list(Line, LagerArguments),
              {atom, Line, get(module)},
              {atom, Line, get(function)},
              {integer, Line, Line}
              ]}]}]}]}]}.

blockify_stmts(Line, Stmts) ->
    {block, Line, Stmts}.

tailify_list(Line, []) -> {nil, Line};
tailify_list(Line, [H|T]) ->
    {cons, Line, H, tailify_list(Line, T)}.
