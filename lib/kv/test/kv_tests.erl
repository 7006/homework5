-module(kv_tests).

-include_lib("eunit/include/eunit.hrl").

process_dictionary_test_() ->
    {
        foreach,
        local,
        fun erlang:erase/0,
        [
            fun process_dictionary_insert/0,
            fun process_dictionary_update/0,
            fun process_dictionary_delete/0,
            fun process_dictionary_read/0
        ]
    }.

process_dictionary_insert() ->
    kv:process_dictionary_insert(5),
    ?assertEqual([{5, 5}, {4, 4}, {3, 3}, {2, 2}, {1, 1}], erlang:get()).

process_dictionary_update() ->
    kv:process_dictionary_insert(5),
    kv:process_dictionary_update(5),
    ?assertEqual([{5, 10}, {4, 8}, {3, 6}, {2, 4}, {1, 2}], erlang:get()).

process_dictionary_delete() ->
    kv:process_dictionary_insert(5),
    kv:process_dictionary_delete(5),
    ?assertEqual([], erlang:get()).

process_dictionary_read() ->
    kv:process_dictionary_insert(5),
    kv:process_dictionary_read(5),
    ?assertEqual([{5, 5}, {4, 4}, {3, 3}, {2, 2}, {1, 1}], erlang:get()).

dict_test_() ->
    [
        fun dict_insert/0,
        fun dict_update/0,
        fun dict_delete/0,
        fun dict_read/0
    ].

dict_insert() ->
    Dict = kv:dict_insert(5, kv:dict_new()),
    ?assertEqual([{3, 3}, {2, 2}, {5, 5}, {1, 1}, {4, 4}], dict:to_list(Dict)).

dict_update() ->
    Dict = kv:dict_insert(5, kv:dict_new()),
    Dict2 = kv:dict_update(5, Dict),
    ?assertEqual([{3, 6}, {2, 4}, {5, 10}, {1, 2}, {4, 8}], dict:to_list(Dict2)).

dict_delete() ->
    Dict = kv:dict_insert(5, kv:dict_new()),
    Dict2 = kv:dict_delete(5, Dict),
    ?assertEqual([], dict:to_list(Dict2)).

dict_read() ->
    Dict = kv:dict_insert(5, kv:dict_new()),
    Dict2 = kv:dict_read(5, Dict),
    ?assertEqual([{3, 3}, {2, 2}, {5, 5}, {1, 1}, {4, 4}], dict:to_list(Dict2)).

map_test_() ->
    [
        fun map_insert/0,
        fun map_update/0,
        fun map_delete/0,
        fun map_read/0
    ].

map_insert() ->
    Map = kv:map_insert(5, kv:map_new()),
    ?assertEqual(#{1 => 1, 2 => 2, 3 => 3, 4 => 4, 5 => 5}, Map).

map_update() ->
    Map = kv:map_insert(5, kv:map_new()),
    Map2 = kv:map_update(5, Map),
    ?assertEqual(#{1 => 2, 2 => 4, 3 => 6, 4 => 8, 5 => 10}, Map2).

map_delete() ->
    Map = kv:map_insert(5, kv:map_new()),
    Map2 = kv:map_delete(5, Map),
    ?assertEqual(#{}, Map2).

map_read() ->
    Map = kv:map_insert(5, kv:map_new()),
    Map2 = kv:map_read(5, Map),
    ?assertEqual(#{1 => 1, 2 => 2, 3 => 3, 4 => 4, 5 => 5}, Map2).

ets_test_() ->
    [
        fun ets_insert/0,
        fun ets_update/0,
        fun ets_delete/0,
        fun ets_read/0
    ].

ets_insert() ->
    Table = kv:ets_new(),
    Table = kv:ets_insert(5, Table),
    ?assertEqual([{4, 4}, {1, 1}, {3, 3}, {2, 2}, {5, 5}], ets:tab2list(Table)).

ets_update() ->
    Table = kv:ets_new(),
    Table = kv:ets_insert(5, Table),
    Table = kv:ets_update(5, Table),
    ?assertEqual([{4, 8}, {1, 2}, {3, 6}, {2, 4}, {5, 10}], ets:tab2list(Table)).

ets_delete() ->
    Table = kv:ets_new(),
    Table = kv:ets_insert(5, Table),
    Table = kv:ets_delete(5, Table),
    ?assertEqual([], ets:tab2list(Table)).

ets_read() ->
    Table = kv:ets_new(),
    Table = kv:ets_insert(5, Table),
    Table = kv:ets_read(5, Table),
    ?assertEqual([{4, 4}, {1, 1}, {3, 3}, {2, 2}, {5, 5}], ets:tab2list(Table)).

proplist_test_() ->
    [
        fun proplist_insert/0,
        fun proplist_update/0,
        fun proplist_delete/0,
        fun proplist_read/0
    ].

proplist_insert() ->
    List = kv:proplist_insert(5, kv:proplist_new()),
    ?assertEqual([{1, 1}, {2, 2}, {3, 3}, {4, 4}, {5, 5}], List).

proplist_update() ->
    List = kv:proplist_insert(5, kv:proplist_new()),
    List2 = kv:proplist_update(5, List),
    ?assertEqual([{1, 2}, {2, 4}, {3, 6}, {4, 8}, {5, 10}], List2).

proplist_delete() ->
    List = kv:proplist_insert(5, kv:proplist_new()),
    List2 = kv:proplist_delete(5, List),
    ?assertEqual([], List2).

proplist_read() ->
    List = kv:proplist_insert(5, kv:proplist_new()),
    List2 = kv:proplist_read(5, List),
    ?assertEqual([{1, 1}, {2, 2}, {3, 3}, {4, 4}, {5, 5}], List2).
