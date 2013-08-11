-module(exp).

%%%===================================================================
%%% Api
%%%===================================================================
-export([parser/1,
         evaluator/1,
         pretty_printer/1,
         compiler/1,
         simulator/1,
         simplifier/1]).

%% parse input string to Erlang exp format
%% InStr - list of chars of expression.
%%         [Ex: ((2+3)-4), 4, ~((2*3)+(3*4))]
%% retrun exps
%% XXX: parser doesn't work properly on not fully bracketed expression
parser(InStr) ->
    %% XXX: toooo ugly, I'm sure
    %% it can be solved in a much smart way
    {Ast, _} = parse_impl(stream(InStr)),
    Ast.

parse_impl({eos, _Next}) -> ok;
parse_impl({H, Next}) ->
    case H of
        $( ->
            {Lh, OpSt} = parse_impl(Next()),
            {Op, RhSt} = parse_op(OpSt),
            {Rh, {$), EndIt}} = parse_impl(RhSt),
            {{Op, Lh, Rh}, EndIt()};
        $~ ->
            {Exp, St} = parse_impl(Next()),
            {{inv, Exp}, St};
        _N ->
            parse_num({H, Next})
    end.

%% stream
%% return {H, Fun}, where H - current character on given list, Fun -
%% function, which return {NextCharacter, Fun} and etc
stream([]) -> eos;
stream([H|T]) ->
    {H, fun() -> stream(T) end}.

parse_op({H, Next}) ->
    Op = case H of
             $- -> minus;
             $+ -> plus;
             $* -> prod;
             $/ -> division
         end,
    {Op, Next()}.

parse_num(Stream) ->
    parse_num_acc(Stream, []).

parse_num_acc(Stream, Acc) ->
    case Stream of
        {N, Next} when N >= 48, N =< 57 ->
            parse_num_acc(Next(), [N|Acc]);
        _Other ->
            {{num, list_to_integer(lists:reverse(Acc))}, Stream}
    end.

%% evaluator
%% return evaluated parsed expression
%% Exp - parsed Erlang expression (see: exp:parser)
evaluator(Exp) ->
    case Exp of
        {num, E} -> E;
        {inv, E} -> -evaluator(E);
        {plus, Lh, Rh} -> evaluator(Lh) + evaluator(Rh);
        {minus, Lh, Rh} -> evaluator(Lh) - evaluator(Rh);
        {prod, Lh, Rh} -> evaluator(Lh) * evaluator(Rh);
        {division, Lh, Rh} -> evaluator(Lh) / evaluator(Rh)
    end.

%% pretty_printer
%% return exp into a string representation
%% TODO: make sure that we cann't avoid lists:flatten
pretty_printer(Exp) ->
    case Exp of
        {num, E} -> integer_to_list(E);
        {inv, E} -> lists:flatten(io_lib:format(exp_format(inv), [pretty_printer(E)]));
        {Op, Lh, Rh} -> lists:flatten(io_lib:format(exp_format(Op), [pretty_printer(Lh),
                                                                     pretty_printer(Rh)]))
    end.

%% exp_format
%% return expression format for give tag
%% Tag - input tag (EX: inv, plus, minus, prod, etc.)
exp_format(Tag) ->
    case Tag of
        inv -> "~~~s";
        plus -> "(~s+~s)";
        minus -> "(~s-~s)";
        prod -> "(~s*~s)";
        division -> "(~s/~s)"
    end.

%% compiler
%% transform exp into a sequence of code for a stack machine to
%% evaluate the exp
%% Exp - parsed exp
compiler(Exp) ->
    compiler_acc(Exp, []).

compiler_acc(Exp, Acc) ->
    case Exp of
        {num, N} ->
            [{push, N}|Acc];
        {Op, E} ->
            compiler_acc(E, [Op|Acc]);
        {Op, Lh, Rh} ->
            compiler_acc(Lh, compiler_acc(Rh, [Op|Acc]))
    end.

%% simulator
%% implement expression for the stack machine
%% Stack - given stack with instruction
%%         (EX: {push N}, division, plus, minus and etc.)
%% return value of expression
simulator(Stack) ->
    sim_impl(Stack, []).

sim_impl([], [V]) -> V;
sim_impl([H|T], Stack) ->
    case H of
        {push, N} ->
            sim_impl(T, [N|Stack]);
        inv ->
            [E|ST] = Stack,
            sim_impl(T, [-E|ST]);
        BinOp ->
            [Rh, Lh|ST] = Stack,
            case BinOp of
                division -> sim_impl(T, [Lh/Rh|ST]);
                prod -> sim_impl(T, [Lh*Rh|ST]);
                plus -> sim_impl(T, [Lh+Rh|ST]);
                minus -> sim_impl(T, [Lh-Rh|ST])
            end
    end.

%% simplifier
%% simplify an expression and exclude operation which cann't influence
%% on value of expression
%% Exp - parsed expression
%% return simplified expression
simplifier(Exp) ->
    case Exp of
        {Op, Lh, Rh} ->
            simplify({Op, simplifier(Lh), simplifier(Rh)});
        _ -> simplify(Exp)
    end.

simplify(Exp) ->
    case Exp of
        {inv, {num, 0}} -> {num, 0};
        %% prod
        {prod, {num, 0}, _} -> {num, 0};
        {prod, _, {num, 0}} -> {num, 0};
        {prod, E, {num, 1}} -> simplifier(E);
        {prod, {num, 1}, E} -> simplifier(E);
        %% division
        {division, {num, 0}, _} -> {num, 0};
        %% plus
        {plus, {num, 0}, E} -> simplifier(E);
        {plus, E, {num , 0}} -> simplifier(E);
        %% minus
        {minus, E, {num, 0}} -> simplifier(E);
        {num, N} -> {num, N};
        Other -> Other
    end.

%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parser_test_() ->
    [
      ?_assertEqual({minus,{plus,{num,2},{num,3}},{num,4}}, parser("((2+3)-4)")),
      ?_assertEqual({num,4}, parser("4")),
      ?_assertEqual({inv,{plus,{prod,{num,2},{num,3}},{prod,{num,3},{num,4}}}},
                    parser("~((2*3)+(3*4))")),
      ?_assertEqual({inv,{num,1}}, parser("~1")),
      ?_assertError(_Error, parser("-1")),
      ?_assertError(_Error, parser(""))
    ].

stream_eos_test() ->
    ?assertEqual(eos, stream([])).

stream_2_test() ->
    {CH1, S1} = stream("123"),
    ?assertEqual($1, CH1),
    {CH2, S2} = S1(),
    ?assertEqual($2, CH2),
    {CH3, S3} = S2(),
    ?assertEqual($3, CH3),
    ?assertEqual(eos, S3()).

evaluator_test_() ->
    [
     ?_assertEqual((1+1), evaluator(parser("(1+1)"))),
     ?_assertEqual((100+100), evaluator(parser("(100+100)"))),
     ?_assertEqual(((2+3)-4), evaluator(parser("((2+3)-4)"))),
     ?_assertEqual(4, evaluator(parser("4"))),
     ?_assertEqual((-((2*3)+(3*4))), evaluator(parser("~((2*3)+(3*4))"))),
     ?_assertEqual(2/4, evaluator(parser("(2/4)")))
    ].

pretty_printer_test_() ->
    [
     ?_assertEqual("(1+1)", pretty_printer(parser("(1+1)"))),
     ?_assertEqual("~1", pretty_printer(parser("~1"))),
     ?_assertEqual("(1+(1/2))", pretty_printer(parser("(1+(1/2))")))
    ].

compiler_test_() ->
    [
     ?_assertEqual([{push,1}], compiler(parser("1"))),
     ?_assertEqual([{push,1},{push,1},plus],
                   compiler(parser("(1+1)"))),
     ?_assertEqual([{push,1},{push,1},{push,2},division,plus],
                   compiler(parser("(1+(1/2))"))),
     ?_assertEqual([{push,2},{push,3},prod,{push,3},{push,4},prod,plus,inv],
                   compiler(parser("~((2*3)+(3*4))")))
    ].

simulator_test_() ->
    [
     ?_assertEqual(evaluator(parser("(1+1)")),
                   simulator(compiler(parser("(1+1)")))),

     ?_assertEqual(evaluator(parser("(100+100)")),
                   simulator(compiler(parser("(100+100)")))),

     ?_assertEqual(evaluator(parser("((2+3)-4)")),
                   simulator(compiler(parser("((2+3)-4)")))),

     ?_assertEqual(evaluator(parser("4")),
                   simulator(compiler(parser("4")))),

     ?_assertEqual(evaluator(parser("~((2*3)+(3*4))")),
                   simulator(compiler(parser("~((2*3)+(3*4))")))),

     ?_assertEqual(evaluator(parser("(2/4)")),
                   simulator(compiler(parser("(2/4)"))))
    ].

simplifier_test_() ->
    [
     ?_assertEqual("(1+1)",
                   pretty_printer(simplifier(parser("(1+1)")))),

     ?_assertEqual("1",
                   pretty_printer(simplifier(parser("(1+(2*(0*19)))")))),

     ?_assertEqual("0",
                   pretty_printer(simplifier(parser("(1*(2*(0*19)))")))),

     ?_assertEqual("0",
                   pretty_printer(simplifier(parser("(0*(2*(10/19)))")))),

     ?_assertEqual("0",
                   pretty_printer(simplifier(parser("(0*(2*(10/19)))"))))
    ].

-endif.
