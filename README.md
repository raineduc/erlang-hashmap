# Hashmap with open addressing
Министерство науки и высшего образования Российской Федерации федеральное государственное автономное образовательное учреждение высшего образования

«Национальный исследовательский университет ИТМО»

---
__ФПИиКТ, Системное и Прикладное Программное Обеспечение__

__Лабораторная работа №2__

по Функциональному программированию

Выполнил: Хузин Р.Р.

Группа: P34112

Преподаватель: Пенской Александр Владимирович

###### Санкт-Петербург
###### 2022 г.
---

## Описание проблемы

Цель: освоиться с построением пользовательских типов данных, полиморфизмом, рекурсивными алгоритмами и средствами тестирования (unit testing, property-based testing).
В рамках лабораторной работы вам предлагается реализовать одну из предложенных классических структур данных (список, дерево, бинарное дерево, hashmap, граф...).


Требования:

1. Функции:

   * добавление и удаление элементов;
   * фильтрация;
   * отображение (map);
   * свертки (левая и правая);
   * структура должна быть моноидом.
2. Структуры данных должны быть неизменяемыми.
3. Библиотека должна быть протестирована в рамках unit testing.
4. Библиотека должна быть протестирована в рамках property-based тестирования (как минимум 3 свойства, включая свойства монойда).
5. Структура должна быть полиморфной.
6. Требуется использовать идиоматичный для технологии стиль программирования.

## Ключевые элементы реализации с минимальными комментариями

1. __Добавление элемента__
    ```erlang
   add_elem(Key, Value, #map{storage = Array, occupied = Occupied}) ->
       ResizedArray =
           case Occupied / array:size(Array) > ?LOAD_FACTOR of
               true ->
                   grow_array(Array);
               false ->
                   Array
           end,
       case put_elem({Key, Value}, ResizedArray) of
           {changed_value, ReturnedArray} ->
               #map{storage = ReturnedArray, occupied = Occupied};
           {new_value, ReturnedArray} ->
               #map{storage = ReturnedArray, occupied = Occupied + 1}
       end.
    ```
2. __Удаление элемента__
   ```erlang
    remove_elem(_, Map) when Map#map.occupied == 0 ->
        Map;
    remove_elem(Key, Map) ->
        #map{storage = Array, occupied = Occupied} = Map,
        case find_elem_index(Key, Map) of
            not_found ->
                Map;
            Index ->
                #map{storage = array:reset(Index, Array), occupied = Occupied - 1}
        end.
   ```
3. __Фильтрация__
    ```erlang
    filter(Pred, Map) ->
        #map{storage = Array} = Map,
        FilteredArray =
            array:sparse_map(fun(_, Elem) ->
                                case Pred(Elem) of
                                    true ->
                                        Elem;
                                    _ ->
                                        array:default(Array)
                                end
                             end,
                             Array),
        RecalcedOccupied = calc_occupied_values(FilteredArray),
        #map{storage = FilteredArray, occupied = RecalcedOccupied}.
    ```

4. __Отображение (map)__
    ```erlang
    map(Func, Map) ->
        #map{storage = Array, occupied = Occupied} = Map,
        MappedArray =
            array:sparse_map(fun(_, Elem) ->
                                {Key, _} = Elem,
                                {Key, Func(Elem)}
                             end,
                             Array),
        #map{storage = MappedArray, occupied = Occupied}.
    ```

5. __Свертка__
    ```erlang
    fold(Func, InitAcc, Map) ->
        #map{storage = Array} = Map,
        array:sparse_foldl(fun(_, Elem, Acc) -> Func(Elem, Acc) end, InitAcc, Array).
    ```
    Так как хеш-таблица не сохраняет порядок вставки, нет смысла разделять свертку
    на левую и правую
6. __Слияние (операция умножения)__
    ```erlang
    merge(Map1, Map2) ->
        #map{storage = Array2} = Map2,
        array:sparse_foldl(fun(_, {Key2, Value2}, Acc) ->
                              add_elem(Key2, Value2, remove_elem(Key2, Acc))
                           end,
                           Map1,
                           Array2).
    ```
7. __Property-based тестирование__
    ```erlang
    get_property_test_result(Property) ->
        proper:quickcheck(Property, [{numtests, ?PROPERTY_TESTS_AMOUNT}, {to_file, user}]).
    
    %%--------------------------------------------------------------------
    %% PROPERTIES
    %%--------------------------------------------------------------------
    
    prop_insert_invariant() ->
        ?FORALL({L, {K, V}},
                {list({string(), integer()}), {string(), integer()}},
                ?IMPLIES(map_oa:get_value(K, map_oa:from_list(L)) =:= not_found,
                         begin
                             Map = map_oa:from_list(L),
                             New = map_oa:add_elem(K, V, Map),
                             (Map#map.occupied + 1 == New#map.occupied)
                             and (map_oa:get_value(K, New) =:= V)
                         end)).
    
    prop_remove_invariant() ->
        ?FORALL({L, K},
                {list({string(), integer()}), string()},
                ?IMPLIES(map_oa:get_value(K, map_oa:from_list(L)) =/= not_found,
                         begin
                             Map = map_oa:from_list(L),
                             New = map_oa:remove_elem(K, Map),
                             OccupiedCondition =
                                 (New#map.occupied == 0)
                                 or (Map#map.occupied - 1 == New#map.occupied)
                                    and (New#map.occupied > 0),
                             OccupiedCondition and (map_oa:get_value(K, New) =:= not_found)
                         end)).
    
    prop_merge_associativity_invariant() ->
        ?FORALL({LeftList, MiddleList, RightList},
                {list({string(), integer()}), list({string(), integer()}), list({string(), integer()})},
                begin
                    Left = map_oa:from_list(LeftList),
                    Middle = map_oa:from_list(MiddleList),
                    Right = map_oa:from_list(RightList),
                    MergedLeft2Right =
                        map_oa:merge(
                            map_oa:merge(Left, Middle), Right),
                    MergedRight2Left = map_oa:merge(Left, map_oa:merge(Middle, Right)),
                    map_oa:is_equal(MergedLeft2Right, MergedRight2Left)
                end).
    
    prop_merge_neutral_elem_invariant() ->
        ?FORALL(L,
                list({string(), integer()}),
                begin
                    EmptyMap = map_oa:new(),
                    Map = map_oa:from_list(L),
                    MergedLeft = map_oa:merge(Map, EmptyMap),
                    MergedRight = map_oa:merge(EmptyMap, Map),
                    map_oa:is_equal(MergedLeft, Map) and map_oa:is_equal(MergedRight, Map)
                end).
    
    %%--------------------------------------------------------------------
    %% TESTS
    %%--------------------------------------------------------------------
    
    test_properties(_) ->
        ?assert(get_property_test_result(prop_insert_invariant())),
        ?assert(get_property_test_result(prop_remove_invariant())),
        ?assert(get_property_test_result(prop_merge_associativity_invariant())),
        ?assert(get_property_test_result(prop_merge_neutral_elem_invariant())).
    ```