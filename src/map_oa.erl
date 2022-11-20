-module(map_oa).

-record(map, {storage, occupied}).
-opaque map_oa(Key, Value) :: #map{storage :: array:array({Key, Value}), occupied :: integer()}.

-export([new/0, is_map_oa/1, add_elem/3, remove_elem/2, filter/2, fold/3, map/2, merge/2]).
-export_type([map_oa/2]).

-define(LOAD_FACTOR, 0.75).
-define(GROW_FACTOR, 2).
-define(INIT_CAPACITY, 8).

new() -> array:new(?INIT_CAPACITY).

is_map_oa(#map{storage = Array, occupied = Occupied}) when is_integer(Occupied) ->
    array:is_array(Array);
is_map_oa(_) ->
  false.

add_elem(Key, Value, #map{storage = Array, occupied = Occupied}) ->
  ResizedArray = case Occupied / size(Array) > 0.75 of
                 true -> grow_array(Array);
                 false -> Array
               end,
  put_elem({Key, Value}, ResizedArray),
  #map{storage = ResizedArray, occupied = Occupied + 1}.

remove_elem(_, Map) when Map#map.occupied == 0 -> Map;
remove_elem(Key, Map) ->
  #map{storage = Array, occupied = Occupied} = Map,
  case find_elem(Key, Array) of
    not_found -> Map;
    Index -> #map{storage = array:reset(Index, Array), occupied = Occupied - 1}
  end.

filter(Pred, Map) ->
  #map{storage = Array} = Map,
  map(
    fun(_, Elem) ->
      case Pred(Elem) of
        true -> Elem;
        _ -> array:default(Array)
      end
    end,
    Map
  ).

map(Func, Map) ->
  #map{storage = Array, occupied = _} = Map,
  FilteredArray = array:sparse_map(
    Func,
    Array
  ),
  RecalcedOccupied = calc_occupied_values(FilteredArray),
  #map{storage = FilteredArray, occupied = RecalcedOccupied}.

fold(Func, InitAcc, Map) ->
  #map{storage = Array, occupied = _} = Map,
  array:sparse_foldl(
    Func,
    InitAcc,
    Array
  ).

%% Merge algorithm equals to Python's 3.9 merge operator |,
%% so if Map1 and Map2 have two equal keys, value of Map2 is stored
merge(Map1, Map2) ->
  merge(Map1, Map2, 0).

merge(Map1, Map2, Map2Index) when Map2Index < size(Map2#map.storage) ->
  {Key2, Value2} = Map2#map.storage,
  UpdatedMap = add_elem(Key2, Value2, remove_elem(Key2, Map1)),
  merge(UpdatedMap, Map2, Map2Index + 1);
merge(UpdatedMap, _, _) -> UpdatedMap.





calc_occupied_values(Array) ->
  array:sparse_foldl(fun(_, _, Acc) -> Acc + 1 end, 0, Array).

put_elem(Elem, Array) ->
  {Key, _} = Elem,
  StartIndex = calc_hash(Key, Array),
  set_elem(Elem, Array, StartIndex).

set_elem(Elem, Array, Index) when Index < size(Array) ->
  Default = array:default(Array),
  case array:get(Index, Array) of
    Default -> array:set(Index, Elem, Array);
    Elem -> Array;
    _ -> set_elem(Elem, Array, Index + 1)
  end;
%% cyclic iteration
set_elem(Elem, Array, Index) -> set_elem(Elem, Index rem size(Array), Array).

find_elem(Elem, Array) ->
  {Key, _} = Elem,
  StartIndex = calc_hash(Key, Array),
  find_elem(Elem, Array, StartIndex, 1).

find_elem(_, Array, _, Iteration) when Iteration > size(Array) -> not_found;
find_elem(Elem, Array, Index, Iteration) when Index < size(Array) ->
  case array:get(Index, Array) of
    Elem -> Index;
    _ -> find_elem(Elem, Array, Index + 1, Iteration + 1)
  end;
find_elem(Elem, Array, Index, Iteration) -> find_elem(Elem, Array, Index rem size(Array), Iteration).

calc_hash(Key, Array) ->
  erlang:phash2(Key, size(Array)).

grow_array(Array) ->
  NewArray = array:new(size(Array) * ?GROW_FACTOR),
  rearrange_elems(Array, NewArray, 0).

rearrange_elems(OldArray, NewArray, Index) when Index < size(OldArray) ->
  Default = array:default(OldArray),
  NextArray = case array:get(Index, OldArray) of
    Default -> NewArray;
    Elem -> put_elem(Elem, NewArray)
  end,
  rearrange_elems(OldArray, NextArray, Index + 1);

rearrange_elems(_, NewArray, _) -> NewArray.