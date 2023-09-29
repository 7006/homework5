# Реалізації сховища типу ключ-значення

- [proplist](https://www.erlang.org/doc/man/proplists)
- [map](https://www.erlang.org/doc/man/maps.html)
- [ets](https://www.erlang.org/doc/man/ets)
- [process dictionary](https://erlang.org/course/advanced.html#dict)
- [dict](https://www.erlang.org/doc/man/dict)
- [record](https://www.erlang.org/doc/reference_manual/records.html)

# Ключові відмінності

|                  |Спільне використання різними процесами|Зберігання великих обсягів даних|Потужна мова запитів|Динамічний розмір сховища|Синтаксичний цукор|Реалізація|
|------------------|-----|-----|-----|-----|-----|--------|
|proplist          | ні  | ні  | ні  | так | ні  |stdlib  |
|map               | ні  | ні  | ні  | так | ні  |BEAM VM |
|ets               | так | так | так | так | ні  |BEAM VM |
|process dictionary| ні  | ні  | ні  | так | ні  |BEAM VM |
|dict              | ні  | ні  | ні  | так | ні  |stdlib  |
|record            | ні  | ні  | ні  | ні  | так |BEAM VM |
