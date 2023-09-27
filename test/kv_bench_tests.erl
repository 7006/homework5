-module(kv_bench_tests).

-include_lib("eunit/include/eunit.hrl").

bench(#{name := Name, init := Init, run := Run}) ->
    {Time, _} = timer:tc(Run, [Init()]),
    io:format("~-20s | ~.3f us~n", [Name, Time / 1000]).

bench_test_() ->
    [
        ?_test(Method(N))
     || Method <- [fun bench_insert/1, fun bench_update/1, fun bench_delete/1, fun bench_read/1],
        N <- [10, 1_000, 10_000]
    ].

bench_insert(N) ->
    bench(#{
        name => "process dictionary",
        init => fun() -> _ = erlang:erase() end,
        run => fun(_) -> kv:process_dictionary_insert(N) end
    }),
    bench(#{
        name => "dict",
        init => fun() -> kv:dict_new() end,
        run => fun(Dict) -> kv:dict_insert(N, Dict) end
    }),
    bench(#{
        name => "map",
        init => fun() -> kv:map_new() end,
        run => fun(Map) -> kv:map_insert(N, Map) end
    }),
    bench(#{
        name => "ets",
        init => fun() -> kv:ets_new() end,
        run => fun(Table) -> kv:ets_insert(N, Table) end
    }),
    bench(#{
        name => "proplist",
        init => fun() -> kv:proplist_new() end,
        run => fun(List) -> kv:proplist_insert(N, List) end
    }),

    io:format(user, "~ninsert N ~p~n~s~n", [N, ?capturedOutput]).

bench_update(N) ->
    bench(#{
        name => "process dictionary",
        init => fun() ->
            _ = erlang:erase(),
            kv:process_dictionary_insert(N)
        end,
        run => fun(_) -> kv:process_dictionary_update(N) end
    }),
    bench(#{
        name => "dict",
        init => fun() -> kv:dict_insert(N, kv:dict_new()) end,
        run => fun(Dict) -> kv:dict_update(N, Dict) end
    }),
    bench(#{
        name => "map",
        init => fun() -> kv:map_insert(N, kv:map_new()) end,
        run => fun(Map) -> kv:map_update(N, Map) end
    }),
    bench(#{
        name => "ets",
        init => fun() -> kv:ets_insert(N, kv:ets_new()) end,
        run => fun(Table) -> kv:ets_update(N, Table) end
    }),
    bench(#{
        name => "proplist",
        init => fun() -> kv:proplist_insert(N, kv:proplist_new()) end,
        run => fun(List) -> kv:proplist_update(N, List) end
    }),

    io:format(user, "~nupdate N ~p~n~s~n", [N, ?capturedOutput]).

bench_delete(N) ->
    bench(#{
        name => "process dictionary",
        init => fun() ->
            _ = erlang:erase(),
            kv:process_dictionary_insert(N)
        end,
        run => fun(_) -> kv:process_dictionary_delete(N) end
    }),
    bench(#{
        name => "dict",
        init => fun() -> kv:dict_insert(N, kv:dict_new()) end,
        run => fun(Dict) -> kv:dict_delete(N, Dict) end
    }),
    bench(#{
        name => "map",
        init => fun() -> kv:map_insert(N, kv:map_new()) end,
        run => fun(Map) -> kv:map_delete(N, Map) end
    }),
    bench(#{
        name => "ets",
        init => fun() -> kv:ets_insert(N, kv:ets_new()) end,
        run => fun(Table) -> kv:ets_update(N, Table) end
    }),
    bench(#{
        name => "proplist",
        init => fun() -> kv:proplist_insert(N, kv:proplist_new()) end,
        run => fun(List) -> kv:proplist_delete(N, List) end
    }),

    io:format(user, "~ndelete N ~p~n~s~n", [N, ?capturedOutput]).

bench_read(N) ->
    bench(#{
        name => "process dictionary",
        init => fun() ->
            _ = erlang:erase(),
            kv:process_dictionary_insert(N)
        end,
        run => fun(_) -> kv:process_dictionary_read(N) end
    }),
    bench(#{
        name => "dict",
        init => fun() -> kv:dict_insert(N, kv:dict_new()) end,
        run => fun(Dict) -> kv:dict_read(N, Dict) end
    }),
    bench(#{
        name => "map",
        init => fun() -> kv:map_insert(N, kv:map_new()) end,
        run => fun(Map) -> kv:map_read(N, Map) end
    }),
    bench(#{
        name => "ets",
        init => fun() -> kv:ets_insert(N, kv:ets_new()) end,
        run => fun(Table) -> kv:ets_read(N, Table) end
    }),
    bench(#{
        name => "proplist",
        init => fun() -> kv:proplist_insert(N, kv:proplist_new()) end,
        run => fun(List) -> kv:proplist_read(N, List) end
    }),

    io:format(user, "~nread N ~p~n~s~n", [N, ?capturedOutput]).
