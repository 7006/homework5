# механізми збереження пар ключ-значення

- [proplists](https://www.erlang.org/doc/man/proplists)
- [map](https://www.erlang.org/doc/man/maps.html)
- [ets](https://www.erlang.org/doc/man/ets)
- [process dictionary](https://erlang.org/course/advanced.html#dict)
- [dict](https://www.erlang.org/doc/man/dict)
- [records](https://www.erlang.org/doc/reference_manual/records.html)


## ключові відмінності

## порівнння швидкості додавання нових елементів

|Add value   |100 elements|1_000 elements|1_000_000 elements|
|------------|------------|--------------|------------------|
|proplists   |            |              |                  |
|maps        |            |              |                  |
|ets         |            |              |                  |
|process dict|            |              |                  |
|dict        |            |              |                  |

## порівнння швидкості оновлення елементів

|Update value|100 elements|1_000 elements|1_000_000 elements|
|------------|------------|--------------|------------------|
|proplists   |            |              |                  |
|maps        |            |              |                  |
|ets         |            |              |                  |
|process dict|            |              |                  |
|dict        |            |              |                  |

## порівнння швидкості видалення елементів

|Delete value|100 elements|1_000 elements|1_000_000 elements|
|------------|------------|--------------|------------------|
|proplists   |            |              |                  |
|maps        |            |              |                  |
|ets         |            |              |                  |
|process dict|            |              |                  |
|dict        |            |              |                  |

## порівнння швидкості читання елементів

|Read value  |100 elements|1_000 elements|1_000_000 elements|
|------------|------------|--------------|------------------|
|proplists   |            |              |                  |
|maps        |            |              |                  |
|ets         |            |              |                  |
|process dict|            |              |                  |
|dict        |            |              |                  |