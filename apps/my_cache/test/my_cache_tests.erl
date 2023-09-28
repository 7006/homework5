-module(my_cache_tests).

-include_lib("eunit/include/eunit.hrl").

cache_create_test() ->
    my_cache:create(table1),

    ?assertEqual(table1, get_table_option(table1, name)),
    ?assertEqual(true, get_table_option(table1, named_table)),
    ?assertEqual(set, get_table_option(table1, type)).

cache_insert_test() ->
    my_cache:create(table2),

    my_cache:insert(table2, a, 1),
    my_cache:insert(table2, b, 2, 1),
    my_cache:insert(table2, c, 3),
    my_cache:insert(table2, d, 4, 2),

    ?assertMatch([{a, 1}, {b, 2, _}, {c, 3}, {d, 4, _}], table_to_list(table2)),
    ?assertEqual(4, get_table_option(table2, size)).

cache_lookup_test() ->
    my_cache:create(table3),

    my_cache:insert(table3, a, 1),
    my_cache:insert(table3, b, 2, 1),
    my_cache:insert(table3, c, 3),
    my_cache:insert(table3, d, 4, 2),

    ?assertEqual(undefined, my_cache:lookup(table3, non_existent_key)),
    ?assertEqual(1, my_cache:lookup(table3, a)),
    ?assertEqual(4, my_cache:lookup(table3, d)),

    timer:sleep(3_000),

    ?assertEqual(1, my_cache:lookup(table3, a)),
    ?assertEqual(undefined, my_cache:lookup(table3, d)).

cache_delete_obsolete_test() ->
    my_cache:create(table4),

    my_cache:insert(table4, a, 1),
    my_cache:insert(table4, b, 2, 1),
    my_cache:insert(table4, c, 3),
    my_cache:insert(table4, d, 4, 2),

    ?assertMatch([{a, 1}, {b, 2, _}, {c, 3}, {d, 4, _}], table_to_list(table4)),

    timer:sleep(3_000),

    my_cache:delete_obsolete(table4),

    ?assertMatch([{a, 1}, {c, 3}], table_to_list(table4)).

get_table_option(TableName, OptionName) ->
    proplists:get_value(OptionName, ets:info(TableName)).

table_to_list(TableName) ->
    lists:reverse(ets:tab2list(TableName)).
