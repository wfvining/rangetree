%% @doc n-dimensional range trees.
%%
%% @copyright 2023 Will Vining <wfv@vining.dev>
-module(rangetree).

-export([new/1, query/3]).

-type rangetree() :: array:array({node, float(), nil | rangetree()} |
                                 {leaf, float(), pos_integer()}).

-spec new([[float()]]) -> rangetree().
new(Points) ->
    new_tree(lists:enumerate(Points)).

-spec query(Min :: [float()], Max :: [float()], Tree :: rangetree()) ->
          [pos_integer()].
query([], [], _) -> [];
query([Min|MinRest], [Max|MaxRest], Tree) ->
    VSplit = find_vsplit(Min, Max, Tree, 0),
    case array:get(VSplit, Tree) of
        {leaf, X, N} when Min =< X, X =< Max ->
            [N];
        {leaf, _, _} ->
            [];
        {node, _, Aux} ->
            Lower = collect_right(Min, Tree, 2 * VSplit + 1),
            Upper = collect_left(Max, Tree, 2 * VSplit + 2),
            AuxI = sets:from_list(query(MinRest, MaxRest, Aux)),
            D = sets:from_list(Lower ++ Upper),
            sets:to_list(sets:intersection([AuxI, D]))
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

new_tree(Points) ->
    L = lists:sort(
          fun ({_, [X|_]}, {_, [Y|_]}) ->
                  X =< Y
          end,
          Points),
    TList = build_tree(0, L),
    T = lists:keysort(1, lists:flatten(TList)),
    array:fix(array:from_orddict(T, undefined)).

split(List) ->
    split(length(List) div 2, List, []).

split(0, [{_, [X|_]} = H|List], [{_, [X|_]}] = L) ->
    split(0, List, [H|L]);
split(0, Right, Left) ->
    {lists:reverse(Left), Right};
split(N, [H|Rest], L) ->
    split(N - 1, Rest, [H|L]).

build_tree(RootIndex, []) -> [{RootIndex, undefined}];
build_tree(RootIndex, [{N, [X|_]}]) -> [{RootIndex, {leaf, X, N}}];
build_tree(RootIndex, Points) ->
    {Left, Right} = split(Points),
    LeftTree = build_tree(RootIndex * 2 + 1, Left),
    RightTree = build_tree(RootIndex * 2 + 2, Right),
    {_, [X|_]} = lists:last(Left),
    [{RootIndex, {node, X, build_aux(Points)}}, LeftTree, RightTree].

build_aux([{_, [_]}|_]) -> nil;
build_aux(Points)       -> new_tree([{I, T} || {I, [_|T]} <- Points]).
