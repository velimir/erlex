-module(show_eval).

-compile([export_all]).

-ifndef(show).
-define(SHOW_EVAL(Exp), Exp).
-else.
-define(SHOW_EVAL(Exp), io:format("~p = ~p~n", [??Exp, Exp])).
-endif.

print_hello() ->
    io:format("Hello!~n"), ok.

do() ->
    ?SHOW_EVAL(print_hello()).
