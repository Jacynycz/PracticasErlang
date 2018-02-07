-module(interface).

-export([start/0,stop/0]).

-include("config.hrl").

start() -> 
   spawn(fun() -> main() end).

stop() -> 
    ok.

main() -> 
    welcome_msg(),
    {ok, [X]} = io:fread("Elección : ", "~a"),
    case X of
        '1' ->
            add_contact_prompt(),
            continue_msg();
        '2' ->
            add_info_prompt(),
            continue_msg();
        '3' ->
            check_contact_prompt(),
            continue_msg();
        '0' ->
            ok;
        '9' ->
            peers_info(),
            continue_msg();
        _ -> main()
    end.

clearscr()->
    io:format("\033[2J").

separator_msg() ->
    
io:format("-----------------~n").

welcome_msg() ->
    clearscr(),
    separator_msg(),
    io:format("Bienvenido a la base de datos~n"),
    separator_msg(),
    io:format("Escriba un número para realizar una acción:~n"),
    io:format("1. Añadir contacto~n"),
    io:format("2. Añadir información a contacto~n"),
    io:format("3. Ver Contacto~n"),
    io:format("4. Editar Contacto~n"),
    io:format("9. Ver nodos conectados~n"),
    io:format("0. Salir~n"),
    separator_msg().

continue_msg() ->
    io:format("~n"),
    io:fread("Presiona ENTER para continuar", ""),
    main().

title_msg(Title) ->
    clearscr(),
    separator_msg(),
    io:format("~s~n",[Title]),
    separator_msg().

header_msg(Title) ->
    %separator_msg(),
    io:format("-- ~s --~n",[Title]).

format(Text) ->
    format_nodes_info(Text).

format_nodes_info([]) ->
    ok;

format_nodes_info([{_Addr,[]}|Tail]) ->
    format_nodes_info(Tail);

format_nodes_info([{Addr,Head}|Tail]) ->
    case Addr == node() of
        false  -> 
            separator_msg(),
            io:format("Información en el nodo ~w~n",[Addr]),
            separator_msg();
        _  -> 
            ok
    end,
    format_info(Head,""),
    format_nodes_info(Tail).

format_info([],_) ->
    ok;

format_info([Head|Tail],Leadingchars)  -> 
    case Head of 
        {Field,[{Subfield,Subvalue}|Subtail]}->
            header_msg(Field),format_info([{Subfield,Subvalue}|Subtail],"----");
        {Field, Value} ->
            io:fwrite("~s ~s - ",[Leadingchars,Field]),
            case is_integer(Value) of
                true ->
                    io:format("~w~n",[Value]);
                false ->
                    io:format("~s~n",[Value])
            end
    end,
format_info(Tail,Leadingchars).

peers_info() ->
    Info = daemon:peers_info(),
    title_msg("PEERS DE LA RED"),
    lists:foreach(
      fun ({Status,Server}) -> 
              case Status of 
                  1 -> io:format("El servidor ~w está ONLINE~n",[Server]);
                  0  -> io:format("El servidor ~w está OFFLINE~n",[Server])
              end
      end
                 , Info
     ),
    io:format("-----------------~n").

add_contact_prompt() ->
    title_msg("AÑADIR CONTACTO"),
    io:format("Introduzca los datos del contacto~n"),
    {ok, [Id]} = io:fread("Identificador de contacto: ", "~a"),
    table:add_contact(Id).

add_info_prompt()  -> 
    title_msg("AÑADIR INFORMACIÓN DE CONTACTO"),
    io:format("Introduzca los datos del contacto~n"),
    {ok, [Id]} = io:fread("Identificador de contacto: ", "~a"),
    case table:exists_contact(Id) of
        true  -> 
            add_info_field(Id);
        [{Node,true}|_] -> 
            io:format("El contacto ya extiste en el nodo ~w, creando contacto~n",[Node]),
            table:add_contact(Id),
            add_info_field(Id);
        []  -> 
            io:format("No existe el contacto~n¿Crear contacto ~w?(s/n)~n",[Id]),
            {ok,[Choice]} =  io:fread("Elección: ", "~a"),
            case Choice of
                s  -> 
                    table:add_contact(Id),
                    add_info_field(Id);
                _ -> ok
            end
    end.

add_info_field(Id) -> 
    header_msg("Añadir un campo individual o múltiple"),
    io:format("1. Individual (Ej. Nombre, Apellidos, DNI)~n"),
    io:format("2. Múltiple (Ej. Teléfonos, Correos, Direcciones)~n"),
    {ok,[Choice]} =  io:fread("Elección: ", "~a"),
    case Choice of 
        '1'  -> 
            add_single_field(Id);
       '2' ->
            add_multi_field(Id);
        _ -> 
            ok
    end.

add_single_field(Id)  -> 
    header_msg("Añadir un campo individual al contacto "++atom_to_list(Id)),
    {ok,[Fieldname]} =  io:fread("Nombre del campo: ", "~a"),
    check_field(Id,Fieldname),
    Value =  getline("Valor del campo: "),
    table:add_single_field(Id,Fieldname,Value).

add_multi_field(Id)  -> 
    header_msg("Añadir un campo múltiple al contacto "++atom_to_list(Id)),
    {ok,[Fieldname]} =  io:fread("Nombre del campo: ", "~a"),
    check_field(Id,Fieldname),
    Values = ask_for_values(Id,Fieldname),
    table:add_single_field(Id,Fieldname,Values).

check_field(Id,Field)  -> 
    case table:ask_node(Id,Field) of
         [{Field, Value}] -> 
            io:format("El campo '~w' tiene el valor '~s' en el nodo actual~n",[Field,Value]),
            ok
   end.

check_subfield(_,_,_) ->
    ok.

getline(Text)  -> 
    Line = io:get_line(Text),
    string:trim(Line,trailing,"\n").

ask_for_values(Id,Fieldname) ->
    {ok,[Subfieldname]} =  io:fread("Nombre del subcampo: ", "~a"),
    check_subfield(Id,Fieldname, Subfieldname),
    Subfieldvalue = getline("Valor del subcampo"),
    {ok,[Choice]} =  io:fread("¿Añadir otro valor más? (s/n): ", "~a"),
    case Choice of 
        's'  -> [{Subfieldname,Subfieldvalue}|ask_for_values(Id,Fieldname)];
        _ ->  [{Subfieldname,Subfieldvalue}]
    end.

check_contact_prompt() ->
    title_msg("VER INFORMACIÓN DE CONTACTO"),
    io:format("1. Ver toda la información del contacto~n"),
    io:format("2. Ver un valor concreto del contacto~n"),
    {ok, [X]} = io:fread("Elección: ", "~a"),
    case X of
        '1' ->
            {ok, [Id]} = io:fread("Identificador de contacto: ", "~a"),
            format(table:ask(Id));
        '2'  -> 
            {ok, [Id]} = io:fread("Identificador de contacto: ", "~a"),
            {ok, [Field]} = io:fread("Valor buscado: ", "~a"),
            format(table:ask(Id,Field));
        _ -> ok
        end.

