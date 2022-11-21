%%%-------------------------------------------------------------------
%%% @author hrami
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. нояб. 2022 21:47
%%%-------------------------------------------------------------------
-module(map_oa_unit_SUITE).

-include_lib("erlang_hashmap/src/map_oa.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% API
-export([all/0]).
-export([is_map_func_test/1, add_elem_test/1, grow_test/1, remove_elem_test/1,
         filter_elems_test/1, map_elems_test/1, fold_elems_test/1, merge_test/1,
         merge_associativity_test/1, merge_neutral_elem_test/1]).

all() ->
    [is_map_func_test,
     add_elem_test,
     grow_test,
     remove_elem_test,
     filter_elems_test,
     map_elems_test,
     fold_elems_test,
     merge_test,
     merge_associativity_test,
     merge_neutral_elem_test].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

is_map_func_test(_) ->
    Array = array:from_list([{"a", 1}, {"b", 2}]),

    ?assert(map_oa:is_map_oa(
                map_oa:new())),
    ?assert(not map_oa:is_map_oa({Array, 2})),
    ?assert(not map_oa:is_map_oa({Array, "2"})),
    ?assert(not map_oa:is_map_oa({{"a", 1}, 1})),
    ?assert(not map_oa:is_map_oa([Array, 2])).

add_elem_test(_) ->
    Map = map_oa:new(),
    With1Elem = map_oa:add_elem("a", 1, Map),
    With2Elems = map_oa:add_elem(["b", "c"], atom, With1Elem),
    #map{storage = _, occupied = Occupied} = With2Elems,

    ?assertEqual(Occupied, 2).

grow_test(_) ->
    #map{storage = Array, occupied = Occupied} = add_thousand_elems(map_oa:new(), 0),

    ?assertEqual(Occupied, 1000),
    ?assert(array:size(Array) >= 1000).

remove_elem_test(_) ->
    Map1 = add_thousand_elems(map_oa:new(), 0),
    %%  removed
    Map2 = map_oa:remove_elem(1, Map1),
    %%  removed
    Map3 = map_oa:remove_elem(404, Map2),
    %%  not remove (not exists)
    Map4 = map_oa:remove_elem(1000, Map3),
    #map{storage = _, occupied = Occupied} = Map4,

    ?assertEqual(Occupied, 998),
    ?assertEqual(not_found, map_oa:get_value(1, Map4)),
    ?assertEqual(not_found, map_oa:get_value(404, Map4)),
    ?assertEqual(not_found, map_oa:get_value(1000, Map4)).

filter_elems_test(_) ->
    Map1 = add_thousand_elems(map_oa:new(), 0),
    Map2 =
        map_oa:filter(fun({Key, Value}) -> (Key == 1) or (Value == 999) or (Value == 501) end,
                      Map1),
    Map3 = map_oa:filter(fun({Key, Value}) -> (Key == -1) or (Value == 1000) end, Map1),

    ?assertEqual(3, Map2#map.occupied),
    ?assertEqual(0, Map3#map.occupied),
    ?assertEqual(1, map_oa:get_value(1, Map2)),
    ?assertEqual(999, map_oa:get_value(999, Map2)),
    ?assertEqual(501, map_oa:get_value(501, Map2)),
    ?assertEqual(not_found, map_oa:get_value(502, Map2)),
    ?assertEqual(not_found, map_oa:get_value(1, Map3)).

map_elems_test(_) ->
    Map1 = add_thousand_elems(map_oa:new(), 0),
    Map2 = map_oa:map(fun({Key, Value}) -> Value * 2 + Key end, Map1),
    #map{storage = _, occupied = Occupied} = Map2,

    ?assertEqual(Occupied, 1000),
    ?assertEqual(3, map_oa:get_value(1, Map2)),
    ?assertEqual(1533, map_oa:get_value(511, Map2)),
    ?assertEqual(2997, map_oa:get_value(999, Map2)).

fold_elems_test(_) ->
    Map1 = add_thousand_elems(map_oa:new(), 0),
    Folded = map_oa:fold(fun({_, Value}, Acc) -> Acc + Value end, 0, Map1),
    ?assertEqual(499500, Folded).

merge_test(_) ->
    Map1 = add_thousand_elems(map_oa:new(), 0),
    Right = map_oa:map(fun({_, Value}) -> Value * 2 end, Map1),
    Left = map_oa:filter(fun({_, Value}) -> Value rem 2 == 0 end, Map1),
    Merged = map_oa:merge(Left, Right),
    #map{occupied = Occupied} = Merged,

    ?assertEqual(1000, Occupied),
    ?assertEqual(1002, map_oa:get_value(501, Right)),
    ?assertEqual(500, map_oa:get_value(500, Left)).

merge_associativity_test(_) ->
    Map1 = add_thousand_elems(map_oa:new(), 0),
    Left = map_oa:filter(fun({Key, _}) -> Key rem 2 == 0 end, Map1),
    Map2 = map_oa:map(fun({_, Value}) -> Value * 2 end, Map1),
    %%  With shared key 2
    Right = map_oa:filter(fun({Key, _}) -> (Key rem 2 == 1) or (Key == 2) end, Map2),
    Middle = map_oa:filter(fun({Key, _}) -> Key rem 3 == 2 end, Map2),
    MergedLeft2Right = map_oa:merge(Left, map_oa:merge(Middle, Right)),
    MergedRight2Left =
        map_oa:merge(
            map_oa:merge(Left, Middle), Right),

    ?assert(map_oa:is_equal(MergedLeft2Right, MergedRight2Left)).

merge_neutral_elem_test(_) ->
    Map1 = add_thousand_elems(map_oa:new(), 0),
    Map2 = map_oa:from_list([{"", 1}, {"", 2}]),
    EmptyMap = map_oa:new(),
    MergedLeft = map_oa:merge(Map1, EmptyMap),
    MergedRight = map_oa:merge(EmptyMap, Map1),

    ?assert(map_oa:is_equal(Map2, map_oa:merge(Map2, EmptyMap))),
    ?assert(map_oa:is_equal(Map2, map_oa:merge(EmptyMap, Map2))),
    ?assert(map_oa:is_equal(Map1, MergedLeft)),
    ?assert(map_oa:is_equal(MergedLeft, MergedRight)).

add_thousand_elems(Map, Count) when Count < 1000 ->
    add_thousand_elems(map_oa:add_elem(Count, Count, Map), Count + 1);
add_thousand_elems(Map, _) ->
    Map.
