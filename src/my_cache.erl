-module(my_cache).

-export([
    create/1,
    insert/3,
    insert/4,
    lookup/2,
    delete_obsolete/1
]).

-include_lib("stdlib/include/ms_transform.hrl").

%% Створюємо таблицю з кешом
create(TableName) ->
    ets:new(TableName, [named_table, set]),
    ok.

%% Додаємо данні без обмеження часу зберігання
insert(TableName, Key, Value) ->
    ets:insert(TableName, {Key, Value}),
    ok.

%% Додаємо данні з обмеженням часу зберігання
%% Останній аргумент це час зберігання запису
%% Вказується в секундах
insert(TableName, Key, Value, Ttl) when is_integer(Ttl), Ttl > 0 ->
    ExpiresAt = now_in_seconds() + Ttl,
    ets:insert(TableName, {Key, Value, ExpiresAt}),
    ok.

%% Функція має повертати значення якщо воно є і не застаріло
%% або undefined якщо значення відсутнє або воно застаріло
lookup(TableName, Key) ->
    case ets:lookup(TableName, Key) of
        [{Key, Value} | _] ->
            Value;
        [{Key, Value, ExpiresAt} | _] ->
            Expired = ExpiresAt < now_in_seconds(),
            case Expired of
                true ->
                    undefined;
                false ->
                    Value
            end;
        [] ->
            undefined
    end.

%% Функція має видаляти данні що застаріли
%% проте досі зберігаються в табличці
delete_obsolete(TableName) ->
    Now = now_in_seconds(),
    MatchSpec = ets:fun2ms(fun({_, _, ExpiresAt}) when ExpiresAt < Now -> true end),
    ets:select_delete(TableName, MatchSpec),
    ok.

now_in_seconds() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()).
