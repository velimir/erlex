%% TODO: add tests

%% simple tests for this module can be taken from book:
%% Db = dblists:new().
%% []
%% Db1 = dblists:write(francesco, london, Db).
%% [{francesco,london}]
%% Db2 = dblists:write(lelle, stockholm, Db1).
%% [{lelle,stockholm},{francesco,london}]
%% dblists:read(francesco, Db2).
%% {ok,london}
%% Db3 = dblists:write(joern, stockholm, Db2).
%% [{joern,stockholm},{lelle,stockholm},{francesco,london}]
%% dblists:read(ola, Db3).
%% {error,instance}
%% dblists:match(stockholm, Db3).
%% [joern,lelle]
%% Db4 = dblists:delete(lelle, Db3).
%% [{joern,stockholm},{francesco,london}]
%% dblists:match(stockholm, Db4).
%% [joern]

-module(dblists).
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).

%% 3.4 Database handling using Lists 
%% Interface
%% db:new() ⇒ Db.
%% db:destroy(Db) ⇒ ok.
%% db:write(Key, Element, Db) ⇒ NewDb.
%% db:delete(Key, Db) ⇒ NewDb.
%% db:read(Key, Db) ⇒ {ok, Element} | {error, instance}.
%% db:match(Element, Db) ⇒ [Key1, ..., KeyN].

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
