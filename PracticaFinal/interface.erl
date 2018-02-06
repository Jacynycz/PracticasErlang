-module(interface).

-export([start/0,stop/0]).

-include("config.hrl").

start() -> 
   spawn(fun() -> main() end).

stop() -> 
    ok.

main() -> 
    welcome_msg(),
    {ok, [X]} = io:fread("Elección : ", "~d"),
    case X of
        1 ->
            add_contact_prompt(),
            continue_msg();
        2 ->
            add_info_prompt(),
            continue_msg();
        3 ->
            check_contact_prompt(),
            continue_msg();
        0 ->
            ok;
        9 ->
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
    io:format("--- ~s ---~n",[Title]).

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
    format_info(Head),
    format_nodes_info(Tail).

format_info([]) ->
    ok;

format_info([Head|Tail])  -> 
    case Head of 
        {Field,[{Subfield,Subvalue}|Subtail]}->
            header_msg(Field),format_info([{Subfield,Subvalue}|Subtail]);
        {Field, Value} ->
            io:fwrite("~s - ",[Field]),
            case is_integer(Value) of
                true ->
                    io:format("~w~n",[Value]);
                false ->
                    io:format("~s~n",[Value])
            end
    end,
format_info(Tail).

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
    add_contact(Id).

add_info_prompt()  -> 
    title_msg("AÑADIR INFORMACIÓN DE CONTACTO"),
    io:format("Introduzca los datos del contacto~n"),
    {ok, [Id]} = io:fread("Identificador de contacto: ", "~a"),
    case table:exists_contact(Id) of
        true  -> 
            header_msg("Añadir un dato individual o múltiple"),
            io:format("1. Individual (Ej. Nombre, Apellidos, DNI)~n"),
            io:format("2. Múltiple (Ej. Teléfonos, Correos, Direcciones)~n"),
            {ok,[Choice]} =  io:fread("Identificador de contacto: ", "~a"),
            {Id,Choice};
        false  -> 
            io:format("No existe el contacto~n")
    end.

check_contact_prompt() ->
    title_msg("VER INFORMACIÓN DE CONTACTO"),
    io:format("1. Ver toda la información del contacto~n"),
    io:format("2. Ver un valor concreto del contacto~n"),
    {ok, [X]} = io:fread("Elección: ", "~d"),
    case X of
        1 ->

            {ok, [Id]} = io:fread("Identificador de contacto: ", "~a"),
            format(ask(Id));
        2  -> 
            {ok, [Id]} = io:fread("Identificador de contacto: ", "~a"),
            {ok, [Field]} = io:fread("Valor buscado: ", "~a"),
            format(ask(Id,Field))
        end.

add_contact(Id)  -> table:add_contact(Id).
ask(Id)  -> table:ask(Id).
ask(Id, Field)  -> table:ask(Id, Field).