-module(dblists).
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
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).

%% new - create empty db
new() ->
    [].

%% destroy - destroy db  
destroy(_) ->
    %% Should we check that it's a Db instance?
    %% we can use tag db_instance for this purpose
    %% Ex: {db_instance, Db}
    ok.

%% write {Key, Element} to Db
%% Key - key of the {key, value} tuple
%% Element - value of the {key, value} tuple
%% Db - database instance
write(Key, Element, Db) -> lists:keystore(Key, 1, Db, {Key, Element}).

%% delete Key from Db
%% Key - key of the {key, value} tuple
%% Db - database instance
delete(Key, Db) -> lists:keydelete(Key, 1, Db).

%% return value for given key, otherwise return {error, instance}
%% Key - key of the {key, value} tuple
%% Db - database instance
read(Key, Db) -> 
    case lists:keyfind(Key, 1, Db) of
        {Key, Value} -> {ok, Value};
        false -> {error, instance}
    end.

%% return all values from Db which is equal to Element
%% Element - value of the {key, value} tuple
%% Db - database instance
match(Element, Db) ->
    case lists:keytake(Element, 2, Db) of
        {value, {Key, Element}, Rest} ->
            [Key|match(Element, Rest)];
        false -> []
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

%% TODO: divide to different tests (read, write, delete)
%%       it can help localize the root of the failed test
op_1_test() ->
    Db1 = write(francesco, london, new()),
    ?assertEqual([{francesco,london}], Db1),
    Db2 = write(lelle, stockholm, Db1),
    %% here is a difference between db implementation
    %% lists opration have another order (ex: append to the end of list)
    ?assertEqual([{francesco,london}, {lelle,stockholm}], Db2),
    ?assertEqual({ok,london}, read(francesco, Db2)),
    Db3 = write(joern, stockholm, Db2),
    ?assertEqual([{francesco,london},{lelle,stockholm},{joern,stockholm}], Db3),
    ?assertEqual({error,instance}, read(ola, Db3)),
    ?assertEqual([lelle,joern], match(stockholm, Db3)),
    Db4 = delete(lelle, Db3),
    ?assertEqual([{francesco,london},{joern,stockholm}], Db4),
    ?assertEqual([joern], match(stockholm, Db4)).

-endif.
