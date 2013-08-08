-module(exp).
%% TODO: add tests
%% TODO: add export 
-compile(export_all).

%% parse input string to Erlang exp format
%% InStr - list of chars of expression.
%%         [Ex: ((2+3)-4), 4, ~((2*3)+(3*4))]
%% retrun exps
parser(InStr) ->
    %% XXX: toooo ugly, I'm sure
    %% that it can be solved in a much smart way
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
