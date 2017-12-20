-module (qsort).
-export ([ordena/1]).

ordena(List) -> ordena(self(), List).
ordena(Padre, []) -> Padre ! {self(), []};
ordena(Padre, [H | T]) ->
    Self = self(),
    Parte_izquierda = spawn(fun() -> ordena(Self, [Y || Y <- T, H >= Y]) end) ,
    Parte_derecha = spawn(fun() -> ordena(Self, [Y || Y <- T, H < Y]) end) ,
    receive
        {Parte_izquierda, []} ->
            receive
                {Parte_derecha, Lista_derecha} -> Padre ! {self(), [H | Lista_derecha]}
            end;
        {Parte_izquierda, Lista_izquierda} ->
            receive
                {Parte_derecha, Lista_derecha} -> Padre ! {self(), Lista_izquierda ++ [H | Lista_derecha]}
            end;
        {Parte_derecha, []} ->
            receive
                {Parte_izquierda, Lista_izquierda} -> Padre ! {self(), Lista_izquierda ++ [H]}
            end;
        {Parte_derecha, Lista_derecha} ->
            receive
                {Parte_izquierda, Lista_izquierda} -> Padre ! {self(), Lista_izquierda ++ [H | Lista_derecha]}
            end
    end.
