%%%-------------------------------------------------------------------
%%% @author hrami
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. нояб. 2022 4:36
%%%-------------------------------------------------------------------
-module(map_oa_proper_SUITE).

-include_lib("erlang_hashmap/src/map_oa.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("proper/include/proper.hrl").

-define(PROPERTY_TESTS_AMOUNT, 100).

%% API
-export([all/0, test_properties/1]).

all() -> [test_properties].

get_property_test_result(Property) -> proper:quickcheck(Property, [{numtests, ?PROPERTY_TESTS_AMOUNT}, {to_file, user}]).

%%--------------------------------------------------------------------
%% PROPERTIES
%%--------------------------------------------------------------------

prop_insert_invariant() ->
  ?FORALL(
    {L, {K, V}},
    {list({string(), integer()}), {string(), integer()}},
    ?IMPLIES(
      map_oa:get_value(K, map_oa:from_list(L)) =:= not_found,
      begin
        Map = map_oa:from_list(L),
        New = map_oa:add_elem(K, V, Map),
        (Map#map.occupied + 1 == New#map.occupied) and (map_oa:get_value(K, New) =:= V)
      end
    )
  ).

prop_remove_invariant() ->
  ?FORALL(
    {L, K},
    {list({string(), integer()}), string()},
    ?IMPLIES(
      map_oa:get_value(K, map_oa:from_list(L)) =/= not_found,
      begin
        Map = map_oa:from_list(L),
        New = map_oa:remove_elem(K, Map),
        OccupiedCondition = (New#map.occupied == 0) or ((Map#map.occupied - 1 == New#map.occupied) and (New#map.occupied > 0)),
        OccupiedCondition and (map_oa:get_value(K, New) =:= not_found)
      end
    )
  ).

prop_merge_associativity_invariant() ->
  ?FORALL(
    {LeftList, MiddleList, RightList},
    {list({string(), integer()}), list({string(), integer()}), list({string(), integer()})},
    begin
      Left = map_oa:from_list(LeftList),
      Middle = map_oa:from_list(MiddleList),
      Right = map_oa:from_list(RightList),
      MergedLeft2Right = map_oa:merge(map_oa:merge(Left, Middle), Right),
      MergedRight2Left = map_oa:merge(Left, map_oa:merge(Middle, Right)),
      map_oa:is_equal(MergedLeft2Right, MergedRight2Left)
    end
  ).

prop_merge_neutral_elem_invariant() ->
  ?FORALL(
    L,
    list({string(), integer()}),
    begin
      EmptyMap = map_oa:new(),
      Map = map_oa:from_list(L),
      MergedLeft = map_oa:merge(Map, EmptyMap),
      MergedRight = map_oa:merge(EmptyMap, Map),
      map_oa:is_equal(MergedLeft, Map) and map_oa:is_equal(MergedRight, Map)
    end
  ).

%%--------------------------------------------------------------------
%% TESTS
%%--------------------------------------------------------------------

test_properties(_) ->
  ?assert(get_property_test_result(prop_insert_invariant())),
  ?assert(get_property_test_result(prop_remove_invariant())),
  ?assert(get_property_test_result(prop_merge_associativity_invariant())),
  ?assert(get_property_test_result(prop_merge_neutral_elem_invariant())).

