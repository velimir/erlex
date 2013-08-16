-module(db).
%%%===================================================================
%%% API
%%% 3.4 Database handling using Lists
%%% Interface
%%% db:new() ⇒ Db.
%%% db:destroy(Db) ⇒ ok.
%%% db:write(Key, Element, Db) ⇒ NewDb.
%%% db:delete(Key, Db) ⇒ NewDb.
%%% db:read(Key, Db) ⇒ {ok, Element} | {error, instance}.
%%% db:match(Element, Db) ⇒ [Key1, ..., KeyN].
%%%===================================================================
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).
-include("data.hrl").
-include("dbg.hrl").
-vsn(1.0).

%% new - create empty db
new() ->
    [].

%% destroy - destroy db
destroy(_) -> ok.


%% write {Key, Element} to Db
%% Key - key of the {key, value} tuple
%% Element - value of the {key, value} tuple
%% Db - database instance
write(Key, Element, Db) ->
    ?DEBUG("writing ~p - ~p to ~p", [Key, Element, Db]),
    case find_val(Key, Db) of
        false -> [#data{key=Key, data=Element}|Db];
        _ -> [#data{key=Key, data=Element}|all_except_key(Key, Db)]
    end.

%% delete Key from Db
%% Key - key of the {key, value} tuple
%% Db - database instance
delete(Key, Db) ->
    ?DEBUG("delete ~p from ~p", [Key, Db]),
    case find_val(Key, Db) of
        false -> Db;
        _ -> all_except_key(Key, Db)
    end.

%% return value for given key, otherwise return {error, instance}
%% Key - key of the {key, value} tuple
%% Db - database instance
read(Key, Db) ->
    ?DEBUG("deleting ~p from ~p", [Key, Db]),
    case find_val(Key, Db) of
        false -> {error, instance};
        Val -> {ok, Val}
    end.

%% return all values from Db which is equal to Element
%% Element - value of the {key, value} tuple
%% Db - database instance
%% TODO: can we avoid reversing?
match(Element, Db) ->
    ?DEBUG("matching ~p on ~p", [Element, Db]),
    reverse(match_acc(Element, Db, [])).

%% private section
match_acc(Val, Db, Out) ->
    case Db of
        [] -> Out;
        [#data{key=Key, data=Val}|T] ->
            match_acc(Val, T, [Key|Out]);
        [_|T] ->
            match_acc(Val, T, Out)
    end.

%% return reversed version of input list
%% List - input list
reverse(List) ->
    reverse_acc(List, []).

reverse_acc([H|T], Acc) ->
    reverse_acc(T, [H|Acc]);
reverse_acc([], Acc) ->
    Acc.

%% find_key - find values for given key, otherwise return false
%% Key - key for pair {Key, Value} stored in Db
%% Db - database
find_val(Key, Db) ->
    ?DEBUG("trying to find ~p in ~p", [Key, Db]),
    case Db of
        [] ->
            false;
        [#data{key=Key, data=Vals}|_] ->
            Vals;
        [_|T] ->
            find_val(Key, T)
    end.

%% return Db list except of first element with key == Key
%% Key - matching key
%% Db - Db list
all_except_key(Key, Db) ->
    all_except_key_acc(Key, Db, []).

all_except_key_acc(Key, Db, NewDb) ->
    case Db of
        [] -> NewDb;
        %% is '++' it ok in this case or it can be avoided?
        [#data{key=Key}|T] -> NewDb ++ T;
        [H|T] -> all_except_key_acc(Key, T, [H|NewDb])
    end.

%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

new_test_() ->
    ?_assertEqual([], new()).

destroy_test_() ->
    [
     ?_assertEqual(ok, destroy([])),
     ?_assertEqual(ok, destroy(new()))
    ].

op_1_test() ->
    Db1 = write(francesco, london, new()),
    ?assertEqual([#data{key=francesco, data=london}], Db1),
    Db2 = write(lelle, stockholm, Db1),
    ?assertEqual([#data{key=lelle, data=stockholm},
                  #data{key=francesco, data=london}], Db2),
    ?assertEqual({ok,london}, read(francesco, Db2)),
    Db3 = write(joern, stockholm, Db2),
    ?assertEqual([#data{key=joern, data=stockholm},
                  #data{key=lelle, data=stockholm},
                  #data{key=francesco, data=london}], Db3),
    ?assertEqual({error,instance}, read(ola, Db3)),
    ?assertEqual([joern,lelle], match(stockholm, Db3)),
    Db4 = delete(lelle, Db3),
    ?assertEqual([#data{key=joern, data=stockholm},
                  #data{key=francesco, data=london}], Db4),
    ?assertEqual([joern], match(stockholm, Db4)).

-endif.
