%Viktor Jacynycz Garc
-module (pract2).
-compile(export_all).

impares([]) -> [];
impares([X|[]]) -> [X];
impares(X) -> [hd(X)]++impares(tl(tl(X))).


ack(0,N) -> N+1;
ack(M,0) when M>0 -> ack(M-1,1);
ack(M,N) -> ack(M-1,ack(M,N-1)).

contiene(_,[]) -> false;
contiene(X,[X|_]) -> true;
contiene(X,[Y|L]) -> contiene(X,L).

contenido(_,[]) -> false;
contenido([],_) -> true;
contenido([X|L],Y) -> contiene(X,Y) and contenido(L,Y).

mismoConjunto(X,Y) -> contenido(X,Y) and contenido(Y,X).

elimina(_,[])->[];
elimina(X,[X|L])->elimina(X,L);
elimina(X,[Y|L])->[Y]++elimina(X,L).

normal([]) -> [];
normal([X|Y]) -> [X]++normal(elimina(X,Y)).

interseccion([],_)->[];
interseccion([X|L],Y) ->
   case contiene(X,Y) of
      true -> [X] ++ interseccion(elimina(X,L),elimina(X,Y));
      false -> interseccion(L,Y)
  end.

esta(X,{X,_,_}) -> true;
esta(Y,{}) -> false;
esta(Y,{X,I,D}) -> esta(Y,I) or esta(Y,D).

nNodos({})->0;
nNodos({_,I,D})->1+nNodos(I)+nNodos(D).

mapTree(F,{}) -> {};
mapTree(F,{A,I,D})->{F(A),mapTree(F,I),mapTree(F,D)}.

sonMultiplos2(_,0)-> true;
sonMultiplos2(X,Y)-> (X rem Y == 0).

sonMultiplos(_,0) -> true;
sonMultiplos(X,Y) -> (X rem Y == 0) or sonMultiplos2(Y,X).

h(X)-> F = fun
 (N) -> sonMultiplos2(N,X)
end.
