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
            add_contact(),
            continue_msg();
        2 ->
            check_contact(),
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
    io:format("1. Añadir Información de contacto~n"),
    io:format("2. Ver Contacto~n"),
    io:format("3. Editar Contacto~n"),
    io:format("9. Ver nodos conectados~n"),
    io:format("0. Salir~n"),
    separator_msg().

continue_msg() ->
    io:format("~n"),
    io:fread("Press Enter to continue", ""),
    main().

title_msg(Title) ->
    clearscr(),
    separator_msg(),
    io:format("~s~n",[Title]),
    separator_msg().

header_msg(Title) ->
    separator_msg(),
    io:format("~s~n",[Title]).

format(Text) ->
    format_l(Text).

format_l([]) ->
    ok;
format_l([Head|Tail]) ->
    case Head of 
        {Field,[{Subfield,Subvalue}|Subtail]}->
            header_msg(Field),format_l([{Subfield,Subvalue}|Subtail]);
        {Field, Value} ->
            io:fwrite("~s - ",[Field]),
            case is_integer(Value) of
                true ->
                    io:format("~w~n",[Value]);
                false ->
                    io:format("~s~n",[Value])
            end
    end,
    format_l(Tail).

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

add_contact() ->
    title_msg("AÑADIR INFORMACIÓN DE CONTACTO"),
    io:format("Introduzca los datos del contacto~n"),
    {ok, [Id]} = io:fread("Identificador de contacto: ", "~a"),
    {ok, [Field]} = io:fread("Nombre del dato: ", "~a"),
    Value = io:get_line("Valor del dato: "),
    add(Id,Field,Value).

check_contact() ->
    title_msg("VER INFORMACIÓN DE CONTACTO"),
    {ok, [Id]} = io:fread("Identificador de contacto: ", "~a"),
    format(ask(Id)).

add(Id,Field,Value)  -> table:insert(Id,Field,Value).
ask(Id)  -> table:ask(Id).
ask(Id, Field)  -> table:ask(Id, Field).
