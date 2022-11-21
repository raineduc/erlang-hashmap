-module(map_oa).

-include("map_oa.hrl").

-opaque map_oa(Key, Value) ::
    #map{storage :: array:array({Key, Value}), occupied :: integer()}.

-export([new/0, from_list/1, is_map_oa/1, get_value/2, add_elem/3, remove_elem/2,
         filter/2, fold/3, map/2, merge/2, is_equal/2]).

-export_type([map_oa/2]).

new() ->
    #map{storage = array:new(?INIT_CAPACITY), occupied = 0}.

from_list(List) ->
    New = new(),
    from_list(New, List).

is_map_oa(#map{storage = Array, occupied = Occupied}) when is_integer(Occupied) ->
    array:is_array(Array);
is_map_oa(_) ->
    false.

add_elem(Key, Value, #map{storage = Array, occupied = Occupied}) ->
    ResizedArray =
        case Occupied / array:size(Array) > ?LOAD_FACTOR of
            true ->
                grow_array(Array);
            false ->
                Array
        end,
    case put_elem({Key, Value}, ResizedArray) of
        {already_exists, ReturnedArray} ->
            #map{storage = ReturnedArray, occupied = Occupied};
        {ok, ReturnedArray} ->
            #map{storage = ReturnedArray, occupied = Occupied + 1}
    end.

get_value(Key, Map) ->
    #map{storage = Array} = Map,
    case find_elem_index(Key, Map) of
        not_found ->
            not_found;
        Index ->
            {_, Value} = array:get(Index, Array),
            Value
    end.

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

map(Func, Map) ->
    #map{storage = Array, occupied = Occupied} = Map,
    MappedArray =
        array:sparse_map(fun(_, Elem) ->
                            {Key, _} = Elem,
                            {Key, Func(Elem)}
                         end,
                         Array),
    #map{storage = MappedArray, occupied = Occupied}.

fold(Func, InitAcc, Map) ->
    #map{storage = Array} = Map,
    array:sparse_foldl(fun(_, Elem, Acc) -> Func(Elem, Acc) end, InitAcc, Array).

%% Merge algorithm equals to Python's 3.9 merge operator |,
%% so if Map1 and Map2 have two equal keys, value of Map2 is stored
merge(Map1, Map2) ->
    #map{storage = Array2} = Map2,
    array:sparse_foldl(fun(_, {Key2, Value2}, Acc) ->
                          add_elem(Key2, Value2, remove_elem(Key2, Acc))
                       end,
                       Map1,
                       Array2).

is_equal(Map1, Map2) when Map1#map.occupied == Map2#map.occupied ->
    #map{storage = Array1} = Map1,
    array:sparse_foldl(fun(_, {Key, Value}, Acc) ->
                          case get_value(Key, Map2) of
                              Value ->
                                  Acc;
                              _ ->
                                  false
                          end
                       end,
                       true,
                       Array1);
is_equal(_, _) ->
    false.

calc_occupied_values(Array) ->
    array:sparse_foldl(fun(_, _, Acc) -> Acc + 1 end, 0, Array).

put_elem(Elem, Array) ->
    {Key, _} = Elem,
    StartIndex = calc_hash(Key, Array),
    set_elem(Elem, Array, StartIndex).

set_elem(Elem, Array, Index) ->
    {Key, _} = Elem,
    case Index < array:size(Array) of
        true ->
            Default = array:default(Array),
            case array:get(Index, Array) of
                Default ->
                    {ok, array:set(Index, Elem, Array)};
                {Key, _} ->
                    {already_exists, Array};
                _ ->
                    set_elem(Elem, Array, Index + 1)
            end;
        %% cyclic iteration
        _ ->
            set_elem(Elem, Array, Index rem array:size(Array))
    end.

find_elem_index(Key, #map{storage = Array, occupied = _}) ->
    StartIndex = calc_hash(Key, Array),
    find_elem_index(Key, Array, StartIndex, 1).

find_elem_index(Key, Array, Index, Iteration) ->
    IterationEnd = Iteration > array:size(Array),
    IndexInBound = Index < array:size(Array),
    case true of
        IterationEnd ->
            not_found;
        IndexInBound ->
            case array:get(Index, Array) of
                {Key, _} ->
                    Index;
                _ ->
                    find_elem_index(Key, Array, Index + 1, Iteration + 1)
            end;
        _ ->
            find_elem_index(Key, Array, Index rem array:size(Array), Iteration)
    end.

calc_hash(Key, Array) ->
    erlang:phash2(Key, array:size(Array)).

grow_array(Array) ->
    NewArray = array:new(array:size(Array) * ?GROW_FACTOR),
    rearrange_elems(Array, NewArray, 0).

rearrange_elems(OldArray, NewArray, Index) ->
    case Index < array:size(OldArray) of
        true ->
            Default = array:default(OldArray),
            NextArray =
                case array:get(Index, OldArray) of
                    Default ->
                        NewArray;
                    Elem ->
                        {_, A} = put_elem(Elem, NewArray),
                        A
                end,
            rearrange_elems(OldArray, NextArray, Index + 1);
        _ ->
            NewArray
    end.

from_list(Map, [Elem | Tail]) ->
    {Key, Value} = Elem,
    NewMap = add_elem(Key, Value, Map),
    from_list(NewMap, Tail);
from_list(Map, []) ->
    Map.
