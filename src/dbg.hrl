-ifdef(debug).
-define(DEBUG(Format, Args),
        io:format("~s.~w: DEBUG: " ++ Format ++ "~n", [ ?MODULE, ?LINE | Args])).
-else.
-define(DEBUG(Format, Args), true).
-endif.
