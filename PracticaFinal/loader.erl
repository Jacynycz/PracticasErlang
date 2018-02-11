-module(loader).
-export([load/1]).

load(Filename) ->
    code:ensure_loaded(table),
                                                % abrimos el archivo en modo lectura y con codificación UTF8 para leer acentos y eñes
    {ok,Handle} = file:open(Filename,[read,{encoding,utf8}]),

                                                % llama a la función get_lines que convierte las líneas a tuplas
    get_data(Handle,'').

get_data(Handle,Last_user)->
    case get_line(Handle) of
        "_contact"->
            Newcontact = list_to_atom(get_line(Handle)),
            table:add_contact(Newcontact),
            get_data(Handle,Newcontact);
        "_single" ->
            {Field,Value} = get_single(Handle),
            table:overwrite_single_field(Last_user,Field,Value),
            get_data(Handle,Last_user);
        "_multi" ->
            Field = list_to_atom(get_line(Handle)),
            Values = get_multi(Handle),
            table:add_multi_field(Last_user,Field,Values),
            get_data(Handle,Last_user);
        _ -> ok
    end.

get_single(Handle)->
    Line = get_line(Handle),
    Tokens = string:tokens(Line,","),
    {list_to_atom(hd(Tokens)),hd(tl(Tokens))}.


get_multi(Handle)->
    Line = get_line(Handle),
    case Line of
        "_endmulti" ->
            [];
        _Otherwise->
            Tokens = string:tokens(Line,","),
            [{list_to_atom(hd(Tokens)),hd(tl(Tokens))}|get_multi(Handle)]
    end.

get_line(Handle)->

    case io:get_line(Handle,"") of

                                                % si estamos al final del fichero, paramos
        eof -> eof;

                                                % en caso contrario, tratamos la línea 
        Line -> string:trim(Line,trailing,"\n")
    end.

