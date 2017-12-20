-module (pract3).
-include_lib("eunit/include/eunit.hrl").
-export ([different/3,differentBis/3, equal/3,positivo/1,diagonal/1,maxLists/2]).
-define(LISTAPRUEBA1,[[-1,2,3],[-1,-2,-3]]).
-define(LISTAPRUEBA2,[[-1,2,3],[-1,-2,-3],[1,2,3]]).
-define (SUMA,fun(X,Y)-> X+Y end).
-define (SUMA_ABS,fun(X,Y) -> abs(X)+abs(Y) end).


different(_,_,[]) -> [];
different(F,G,L) -> lists:filter(fun(X)->lists:foldl(F,0,X) /= lists:foldl(G,0,X) end,L).
different_test() -> [?assertEqual(?LISTAPRUEBA1,different(?SUMA,?SUMA_ABS,?LISTAPRUEBA2))].

differentBis(F,G,L) -> [X || X <- L,lists:foldl(F,0,X) /= lists:foldl(G,0,X)].
differentBis_test() -> [?assertEqual(?LISTAPRUEBA1,differentBis(?SUMA,?SUMA_ABS,?LISTAPRUEBA2))].

equal(F,G,L) -> lists:map(F,L) == lists:map(G,L).
equal_test() -> [?assert(equal(fun erlang:abs/1,fun erlang:abs/1,lists:seq(-10,10)))].

positivo(L) -> [N || {N,V}<-L,is_integer(V) and (V>0)].
positivo_test() -> [?assertEqual([bertoldo],positivo( [{bertoldo, 500}, {herminia,cancelada}, {aniceto,-2000}]))].

diagonal(N) -> [lists:duplicate(X-1, 0)++[1]++lists:duplicate(N-X, 0) || X <- lists:seq(1,N)].

maxLists(LF,LV) -> [lists:foldl(fun erlang:max/2,0,X) || X <- [[Y(Vals) || Y <- LF] || Vals <- LV]].
