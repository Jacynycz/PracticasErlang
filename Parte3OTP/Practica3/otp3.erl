-module(otp3).
-export([carga/0,cargaets/0,consulta1/1,consulta2/1,consulta3/1]).

carga()  -> load("clientes.txt").

cargaets()  -> Data = carga(),
                       Mytable = ets:new(my_table,[set]),
                       lists:map(
                        fun(X)  -> ets:insert(Mytable,X) end,
                         Data),
                       Mytable.

consulta1(Table)  -> C = ets:lookup_element(Table,3,4),
                     io:format("La ciudad del elemento con la clave 3 es: "),
                     C.

consulta2(Table) ->  C= ets:select(Table,[
                                          {
                                            {'_',[$C|'$2'],'_',"Madrid"},[],
                                            [[$C|'$2']]
                                          }
                                         ]),
                     io:format("Los nombres y apellidos de los residentes en madrid cuyo nombre empieza por C son: "),
                     C.

consulta3(Table) ->   C = ets:select(Table,[
                                               {
                                                 {'_','$1','$2','_'},
                                                 [{'=<','$2',30},{'>=','$2',20}],
                                                 ['$1']
                                               }
                                              ]),
                     io:format("Los nombres y apellidos de las personas entre 20 y 30 años (ambos inclusive) son:  "),
                     C.

load(Filename)  -> 

    % abrimos el archivo en modo lectura y con codificación UTF8 para leer acentos y eñes
    {ok,Handle} = file:open(Filename,[read,{encoding,utf8}]),

    % llama a la función get_lines que convierte las líneas a tuplas
    get_lines(Handle).



get_lines(Handle) -> get_lines(Handle,[]).

get_lines(Handle,List) -> 
    case io:get_line(Handle,"") of

        % si estamos al final del fichero, paramos
        eof -> [];

        % en caso contrario, tratamos la línea 
        Line -> 
            
            % eliminamos el salto de línea del final
            Trimmed = string:trim(Line,trailing,"\n"),
            
            % separamos en tokens utilizando el delimitador ','
            Tokens = string:tokens(Trimmed,","),

            % eliminamos los espacios del principio y del final de cada elemento
            Clean = lists:map(
                      fun(X) -> string:trim(X,both," ") end,
                      Tokens),

            % añadimos el elemento a la lista después de convertirlo a tupla
            List++ [tupleize(Clean)] ++ get_lines(Handle)
    end.

tupleize(Tokens)  -> 
    
    % convierte los elementos cuyo índice está en la lista de String a Integer
    % en este caso queremos que los elementos 1 y 3 sean enteros
    Clean = convertNtoint(Tokens,[1,3]),

    % transforma de lista a tupla
    list_to_tuple(Clean).

convertNtoint(L,L2)  -> convertNtoint(L,L2,1).
convertNtoint([],_,_) -> [];
convertNtoint(List,[],_) -> List;

% si estamos en el índice marcado por uno de los elementos a convertir
% - convierte el elemento
% - aumenta el índice
% - elimina la cabeza de los índices a convertir 
convertNtoint([H|T],[N|Rest],Index) when N == Index ->
    {Num,_}=string:to_integer(H),
    [Num|convertNtoint(T,Rest,Index+1)];


% si el índice es menor que los elementos que queremos convertir, lo aumenta
convertNtoint([H|T],[N|Rest],Index) when N > Index -> 
    [H|convertNtoint(T,[N|Rest],Index+1)].


