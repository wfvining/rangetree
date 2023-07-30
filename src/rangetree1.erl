%%% @doc One-dimensional range tree.
%%%
%%% @copyright 2023 Will Vining <wfv@vining.dev>
-module(rangetree1).

-export([new/1, query/3]).

-type rangetree1() :: array:array({node | leaf, float(), pos_integer()}).

-spec new([float()]) -> rangetree1().
new(Points) ->
    L = lists:keysort(2, lists:enumerate(Points)),
    TList = build_tree(0, L),
    T = lists:keysort(1, lists:flatten(TList)),
    array:fix(array:from_orddict(T, undefined)).

-spec query(Min :: float(), Max :: float(), Tree :: rangetree1()) -> [pos_integer()].
query(Min, Max, Tree) when Min =< Max ->
    MinIx = find_min(Min, Tree, 0),
    MaxIx = find_max(Max, Tree, 0),
    collect_range(MinIx, MaxIx, Tree).

collect_range({MinIx, inclusive}, MaxIx, Tree) ->
    collect_range(MinIx, MaxIx, Tree, []);
collect_range({MinIx, exclusive}, MaxIx, Tree) ->
    collect_range(MinIx + 1, MaxIx, Tree, []).

collect_range(MinIx, {MaxIx, _}, _, Acc)
  when MinIx > MaxIx ->
    Acc;
collect_range(MinIx, {MaxIx, inclusive}, Tree, Acc)
  when MinIx =:= MaxIx ->
    {leaf, _, I} = array:get(MinIx, Tree),
    lists:reverse([I|Acc]);
collect_range(MinIx, {MaxIx, exclusive}, _, Acc)
  when MinIx =:= MaxIx ->
    lists:reverse(Acc);
collect_range(MinIx, MaxIx, Tree, Acc) ->
    case array:get(MinIx, Tree) of
        undefined ->
            collect_range(MinIx + 1, MaxIx, Tree, Acc);
        {node, _, _} ->
            collect_range(MinIx + 1, MaxIx, Tree, Acc);
        {leaf, _, I} ->
            collect_range(MinIx + 1, MaxIx, Tree, [I|Acc])
    end.

find_max(Max, Tree, I) ->
    case array:get(I, Tree) of
        {node, X, _} when X >= Max ->
            find_max(Max, Tree, I * 2 + 1);
        {node, X, _} when X < Max ->
            find_max(Max, Tree, I * 2 + 2);
        {leaf, X, _} when X =< Max ->
            {I, inclusive};
        {leaf, X, _} when X > Max ->
            {I, exclusive}
    end.

find_min(Min, Tree, I) ->
    case array:get(I, Tree) of
        {node, X, _} when X < Min ->
            find_min(Min, Tree, I * 2 + 2);
        {node, X, _} when X >= Min ->
            find_min(Min, Tree, I * 2 + 1);
        {leaf, X, _} when X >= Min ->
            {I, inclusive};
        {leaf, X, _} when X < Min ->
            {I, exclusive}
        end.

split(List) ->
    split(length(List) div 2, List, []).

split(0, [{_, X} = H|List], [{_, X}|_] = L) ->
    split(0, List, [H|L]);
split(0, Right, Left) ->
    {lists:reverse(Left), Right};
split(N, [H|Rest], L) ->
    split(N - 1, Rest, [H|L]).

build_tree(RootIndex, []) -> [{RootIndex, undefined}];
build_tree(RootIndex, [{N, X}]) -> [{RootIndex, {leaf, X, N}}];
build_tree(RootIndex, Points) ->
    {Left, Right} = split(Points),
    LeftTree = build_tree(RootIndex * 2 + 1, Left),
    RightTree = build_tree(RootIndex * 2 + 2, Right),
    {_, X} = lists:last(Left),
    [{RootIndex, {node, X, length(Points)}}, LeftTree, RightTree].
