-module(prop_rangetree).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_all_covered_in_range() ->
    ?FORALL({Points, {Min, Max}}, tree_and_query(),
        begin
            T = rangetree:new(Points),
            Covered = rangetree:query(Min, Max, T),
            {CoveredValues, _NotCoveredValues} = partition_values(Points, Covered),
            lists:all(
              fun(P) -> lists:any(
                          fun({X, XMin, XMax}) -> (XMin =< X) and (X =< XMax) end,
                          lists:zip3(P, Min, Max))
              end, CoveredValues)
        end).

prop_all_not_covered_out_of_range() ->
    ?FORALL({Points, {Min, Max}}, tree_and_query(),
        begin
            T = rangetree:new(Points),
            Covered = rangetree:query(Min, Max, T),
            {_CoveredValues, NotCoveredValues} = partition_values(Points, Covered),
            length(Covered) / length(Points),
            lists:all(
              fun(P) -> lists:any(
                          fun({X, XMin, XMax}) -> (X < XMin) or (X > XMax) end,
                          lists:zip3(P, Min, Max))
              end, NotCoveredValues)
        end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
partition_values(Points, Covered) ->
    {[lists:nth(N, Points) || N <- Covered],
     [lists:nth(N, Points) || N <- (lists:seq(1, length(Points)) -- Covered)]}.

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
dimensions() ->
    ?LET(D, integer(), (abs(D) rem 10) + 1).

tree_and_query() ->
    ?LET(Dims, dimensions(), {points(Dims), range(Dims)}).

points(Dims) ->
    ?LET(D, Dims, non_empty(list(point(D)))).

point(Size) ->
    [float() || _ <- lists:seq(1, Size)].

range(Dims) ->
    ?LET({Xs, Ys}, {point(Dims), point(Dims)},
         {lists:zipwith(fun min/2, Xs, Ys), lists:zipwith(fun max/2, Xs, Ys)}).
