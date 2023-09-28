# Механізми збереження пар ключ-значення

- [proplist](https://www.erlang.org/doc/man/proplists)
- [map](https://www.erlang.org/doc/man/maps.html)
- [ets](https://www.erlang.org/doc/man/ets)
- [process dictionary](https://erlang.org/course/advanced.html#dict)
- [dict](https://www.erlang.org/doc/man/dict)
- [records](https://www.erlang.org/doc/reference_manual/records.html)

# Порівнння операцій

## Додавання нових елементів

|                  |10          |1 000         |10 000            |
|------------------|------------|--------------|------------------|
|proplist          |            |              |                  |
|map               |            |              |                  |
|ets               |            |              |                  |
|process dictionary|            |              |                  |
|dict              |            |              |                  |

## Оновлення елементів

|                  |10          |1 000         |10 000            |
|------------------|------------|--------------|------------------|
|proplist          |            |              |                  |
|map               |            |              |                  |
|ets               |            |              |                  |
|process dictionary|            |              |                  |
|dict              |            |              |                  |

## Видалення елементів

|                  |10          |1 000         |10 000            |
|------------------|------------|--------------|------------------|
|proplist          |            |              |                  |
|map               |            |              |                  |
|ets               |            |              |                  |
|process dictionary|            |              |                  |
|dict              |            |              |                  |

## Читання елементів

|                  |10          |1 000         |10 000            |
|------------------|------------|--------------|------------------|
|proplist          |            |              |                  |
|map               |            |              |                  |
|ets               |            |              |                  |
|process dictionary|            |              |                  |
|dict              |            |              |                  |
