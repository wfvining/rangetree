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
    VSplit = find_vsplit(Min, Max, Tree, 0),
    %% Collect all leafs to the right of nodes on the path from VSplit
    %% to VMin
    case array:get(VSplit, Tree) of
        {leaf, X, N} when Min =< X, X =< Max ->
            [N];
        {leaf, _, _} ->
            [];
        {node, _, _} ->
            Lower = collect_right(Min, Tree, 2 * VSplit + 1),
            Upper = collect_left(Max, Tree, 2 * VSplit + 2),
            Lower ++ Upper
    end.

collect_left(Max, Tree, I) ->
    case array:get(I, Tree) of
        {leaf, X, N} when X =< Max ->
            [N];
        {leaf, _, _} ->
            [];
        {node, X, _} when X < Max ->
            collect_left(Max, Tree, 2 * I + 2) ++ leaves(2 * I + 1, Tree);
        {node, X, _} when X >= Max ->
            collect_left(Max, Tree, 2 * I + 1)
    end.

collect_right(Min, Tree, I) ->
    case array:get(I, Tree) of
        {leaf, X, N} when Min =< X ->
            [N];
        {leaf, _, _} ->
            [];
        {node, X, _} when Min =< X ->
            collect_right(Min, Tree, 2 * I + 1) ++ leaves(2 * I + 2, Tree);
        {node, X, _} when Min > X ->
            collect_right(Min, Tree, 2 * I + 2)
    end.

%% Return all leaves below I.
leaves(I, Tree) ->
    case array:get(I, Tree) of
        {leaf, _, N} ->
            [N];
        {node, _, _} ->
            leaves(2 * I + 1, Tree) ++ leaves(2 * I + 2, Tree)
    end.

find_vsplit(Min, Max, Tree, I) ->
    case array:get(I, Tree) of
        {node, X, _} when X < Max, Min =< X ->
            %% The paths diverge here, this is VSplit
            I;
        {node, X, _} when Max =< X ->
            %% the range is contained on the left
            find_vsplit(Min, Max, Tree, 2 * I + 1);
        {node, X, _} when X < Min ->
            %% the range is contained on the right
            find_vsplit(Min, Max, Tree, 2 * I + 2);
        {leaf, _, _} ->
            %% If we reached a leaf then that is VSplit
            I
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
