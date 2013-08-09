%% 3.9 - Indexing
-module(index).
-compile(export_all).

%% raw_doc
%% read file and return raw document (list of lines)
%% FilePath - path to file
raw_doc(FilePath) ->
    {ok, File} = file:open(FilePath, [read]),
    try get_all_lines(File)
    after
        file:close(File)
    end.

get_all_lines(File) ->
    case io:get_line(File, "") of
        eof -> [];
        Line -> [Line -- "\n"|get_all_lines(File)]
    end.

%% doc
%% return list of words
%% input - list of lines
doc([]) -> [];
doc([Line|T]) ->
    string:tokens(Line, " ") ++ doc(T).

%% index_doc
%% create ordered dicionary {Word, [occurrences_in_text]}
%% RawDocTail - raw document (see: func raw_doc)
index_doc(RawDocTail) ->
    index_doc_impl(RawDocTail, 1, orddict:new()).

index_doc_impl([], _, Dict) -> Dict;
index_doc_impl([Line|RawDocTail], LineNumber, Dict) ->
    index_doc_impl(RawDocTail, LineNumber + 1,
                   lists:foldl(fun(Word, D) -> orddict:append(Word, LineNumber, D) end,
                                   Dict, string:tokens(Line, " "))).

%% adjust_index
%% return list of rages
%% Ex: adjust_index([1,1,2,4,5,6,6,98,100,102,102])
%%     [{1,2},{4,6},{98,98},{100,100},{102,102}]
%% Indexes - list of numbers
adjust_index(Indexes) ->
    lists:foldr(fun(El, Out) ->
                        case Out of
                            [] -> [{El, El}];
                            [{Lb, Rb}|T] ->
                                if El >= Lb - 1 -> [{El, Rb}|T];
                                   true ->  [{El, El}, {Lb, Rb}|T]
                                end
                        end
                end,
                [], Indexes).

%% ranges_to_list
%% return string of ranges
%% Ex: ranges_to_list([{1,2},{4,6},{98,98},{100,100},{102,102}])
%%     "1-2,4-6,98-98,100-100,102-102"
%% Ranges - list of ranges
ranges_to_list(Ranges) ->
    string:join(lists:map(
                  fun({Lb, Rb}) -> io_lib:format("~b-~b", [Lb, Rb]) end,
                  Ranges),
                ",").

%% max_key_len
%% return max length of key from given Dict
%% Dict - dictionary (list of tuples)
max_key_len(Dict) ->
    lists:foldr(fun({Key, _}, MaxLen) -> if
                                             length(Key) > MaxLen -> length(Key);
                                             true -> MaxLen
                                         end
                end, 0, Dict).

word_format({Word, Indexes}) ->
    lists:flatten(io_lib:format("~s ~s", [Word, ranges_to_list(adjust_index(Indexes))])).

%% pretty_print
%% return pretty readable words indexes string
%% USAGE: io:fwrite([index:pretty_print(index:index_doc(index:raw_doc(<file_path>)))]).
%% Dict - Dictionary
pretty_print(Dict) ->
    string:join(lists:map(fun word_format/1, Dict), "\n").
