-module(kv).

-export([
    process_dictionary_insert/1,
    process_dictionary_update/1,
    process_dictionary_delete/1,
    process_dictionary_read/1,
    dict_insert/2,
    dict_update/2,
    dict_delete/2,
    dict_read/2,
    dict_new/0,
    map_insert/2,
    map_update/2,
    map_delete/2,
    map_read/2,
    map_new/0,
    ets_insert/2,
    ets_update/2,
    ets_delete/2,
    ets_read/2,
    ets_new/0,
    proplist_insert/2,
    proplist_update/2,
    proplist_delete/2,
    proplist_read/2,
    proplist_new/0
]).

process_dictionary_insert(N) when is_integer(N), N > 0 ->
    undefined = erlang:put(N, N),
    process_dictionary_insert(N - 1);
process_dictionary_insert(0) ->
    ok.

process_dictionary_update(N) when is_integer(N), N > 0 ->
    N = erlang:put(N, double(N)),
    process_dictionary_update(N - 1);
process_dictionary_update(0) ->
    ok.

process_dictionary_delete(N) when is_integer(N), N > 0 ->
    N = erlang:erase(N),
    process_dictionary_delete(N - 1);
process_dictionary_delete(0) ->
    ok.

process_dictionary_read(N) when is_integer(N), N > 0 ->
    N = erlang:get(N),
    process_dictionary_read(N - 1);
process_dictionary_read(0) ->
    ok.

dict_insert(N, Dict) when is_integer(N), N > 0 ->
    Dict2 = dict:store(N, N, Dict),
    dict_insert(N - 1, Dict2);
dict_insert(0, Dict) ->
    Dict.

dict_update(N, Dict) when is_integer(N), N > 0 ->
    Dict2 = dict:update(N, fun double/1, Dict),
    dict_update(N - 1, Dict2);
dict_update(0, Dict) ->
    Dict.

dict_delete(N, Dict) when is_integer(N), N > 0 ->
    Dict2 = dict:erase(N, Dict),
    dict_delete(N - 1, Dict2);
dict_delete(0, Dict) ->
    Dict.

dict_read(N, Dict) when is_integer(N), N > 0 ->
    N = dict:fetch(N, Dict),
    dict_read(N - 1, Dict);
dict_read(0, Dict) ->
    Dict.

dict_new() ->
    dict:new().

map_insert(N, Map) when is_integer(N), N > 0 ->
    Map2 = maps:put(N, N, Map),
    map_insert(N - 1, Map2);
map_insert(0, Map) ->
    Map.

map_update(N, Map) when is_integer(N), N > 0 ->
    Map2 = maps:update_with(N, fun double/1, Map),
    map_update(N - 1, Map2);
map_update(0, Map) ->
    Map.

map_delete(N, Map) when is_integer(N), N > 0 ->
    Map2 = maps:remove(N, Map),
    map_delete(N - 1, Map2);
map_delete(0, Map) ->
    Map.

map_read(N, Map) when is_integer(N), N > 0 ->
    N = maps:get(N, Map),
    map_read(N - 1, Map);
map_read(0, Map) ->
    Map.

map_new() ->
    maps:new().

ets_insert(N, Table) when is_integer(N), N > 0 ->
    true = ets:insert_new(Table, {N, N}),
    ets_insert(N - 1, Table);
ets_insert(0, Table) ->
    Table.

ets_update(N, Table) when is_integer(N), N > 0 ->
    true = ets:update_element(Table, N, {2, double(N)}),
    ets_update(N - 1, Table);
ets_update(0, Table) ->
    Table.

ets_delete(N, Table) when is_integer(N), N > 0 ->
    true = ets:delete(Table, N),
    ets_delete(N - 1, Table);
ets_delete(0, Table) ->
    Table.

ets_read(N, Table) when is_integer(N), N > 0 ->
    N = ets:lookup_element(Table, N, 2),
    ets_read(N - 1, Table);
ets_read(0, Table) ->
    Table.

ets_new() ->
    ets:new(?FUNCTION_NAME, []).

proplist_insert(N, List) when is_integer(N), N > 0 ->
    List2 = [{N, N} | List],
    proplist_insert(N - 1, List2);
proplist_insert(0, List) ->
    List.

proplist_update(N, List) when is_integer(N), N > 0 ->
    List2 = lists:keyreplace(N, 1, List, {N, double(N)}),
    proplist_update(N - 1, List2);
proplist_update(0, List) ->
    List.

proplist_delete(N, List) when is_integer(N), N > 0 ->
    List2 = proplists:delete(N, List),
    proplist_delete(N - 1, List2);
proplist_delete(0, List) ->
    List.

proplist_read(N, List) when is_integer(N), N > 0 ->
    N = proplists:get_value(N, List),
    proplist_read(N - 1, List);
proplist_read(0, List) ->
    List.

proplist_new() ->
    [].

double(N) ->
    N + N.
