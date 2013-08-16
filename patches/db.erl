%% Code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson
%%   modified by Grigory Starinkin, 2013

-module(db).
-export([new/0, destroy/1, write/3, delete/2, read/2, convert/2, code_upgrade/1]).
-include("../src/dbg.hrl").
-include("../src/data.hrl").
-vsn(1.2).

new() -> gb_trees:empty().

write(Key, Data, Db) -> gb_trees:insert(Key, Data, Db).

read(Key, Db) ->
    case gb_trees:lookup(Key, Db) of
        none         -> {error, instance};
        {value, Data} -> {ok, Data}
    end.

destroy(_Db) -> ok.

delete(Key, Db) -> gb_trees:delete(Key, Db).

convert(dict, Dict) ->
    dict(dict:fetch_keys(Dict), Dict, new());
convert(list, DataList) ->
    list(DataList, new());
convert(_, Data) ->
    Data.

dict([Key|Tail], Dict, GbTree) ->
    Data = dict:fetch(Key, Dict),
    NewGbTree  = gb_trees:insert(Key, Data, GbTree),
    dict(Tail, Dict, NewGbTree);
dict([], _, GbTree) -> GbTree.

list([#data{key=Key, data=Data}|Tail], GbTree) ->
    ?DEBUG("adding ~p - ~p to bg_tree", [Key, Data]),
    NewGbTree = gb_trees:insert(Key, Data, GbTree),
    list(Tail, NewGbTree);
list([], GbTree) -> GbTree.
    
code_upgrade(ListVer) -> 
    ?DEBUG("code upgrade from: ~p", [ListVer]),
    convert(list, ListVer).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

code_upgrade_test() ->
    GbTree = code_upgrade([#data{key=francesco, data=london}]),
    ?assertMatch({ok, london}, read(francesco, GbTree)).

-endif.
