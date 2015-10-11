% vim: set expandtab softtabstop=4 shiftwidth=4:
-module(erlwitness_transform).
-export([parse_transform/2]).
-ignore_xref([{parse_transform, 2}]).

%X @doc Derived from basho/lager's lager_transform.erl (under Apache License 2.0)
%X

-include_lib("lager/include/lager.hrl").


%% @private
parse_transform(AST, _Options) ->
    %% .app file should either be in the outdir, or the same dir as the source file
    %guess_application(proplists:get_value(outdir, Options), hd(AST)),
    walk_ast([], AST).

walk_ast(Acc, []) ->
    lists:reverse(Acc);
walk_ast(Acc, [{attribute, _, module, {Module, _PmodArgs}}=H|T]) ->
    %% A wild parameterized module appears!
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

transform_statement({call, Line, {remote, _Line1, {atom, _Line2, Module},
                                  {atom, _Line3, Function}}, Arguments0} = Stmt) 
->
    case lists:member(Function, ?LEVELS) orelse lists:keymember(Function, 1, ?LEVELS_UNSAFE) of
        true when Module /= erlwitness ->
            % Transform our copy and place it before the original call 
            % (it must be place before in order to avoid conflicts derived from common line numbers.)
            blockify_stmts(Line, [do_transform(Line, Function, Arguments0),
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

do_transform(Line, Function, Arguments0) ->
    % 1) Wrap our potential lager call into a checker for watching
    % 2) Overwrite sink with our own
    LagerStmt = {call, Line, 
                 {remote, Line, {atom, Line, erlwitness}, {atom, Line, Function}},
                 Arguments0},

    FunifiedLagerStmt = funify_stmts(Line, [LagerStmt]),

    EntityVar = list_to_atom("ErlWitness_LagerWrapper_SelfEntity_" ++ integer_to_list(Line)),
    {'case',Line,
     {call,Line,
      {remote,Line,{atom,Line,erlwitness_entity},{atom,Line,get}},
      []},
     [{clause,Line,[{atom,Line,undefined}],[],[{atom,Line,ok}]},
      {clause,Line,
       [{tuple,Line,[{atom,Line,value},{var,Line,EntityVar}]}],
       [],
       [{'case',Line,
         {call,Line,
          {remote,Line,
           {atom,Line,erlwitness_lobby},
           {atom,Line,is_entity_watched}},
          [{var,Line,EntityVar}]},
         [{clause,Line,[{atom,Line,false}],[],[{atom,Line,ok}]},
          {clause,Line,[{atom,Line,true}],[],[FunifiedLagerStmt]}]}]}]}.

funify_stmts(Line, Stmts) ->
    {call, Line, {'fun',Line, {clauses,[{clause,Line,[],[], Stmts}]}}, []}.

blockify_stmts(Line, Stmts) ->
    {block, Line, Stmts}.
