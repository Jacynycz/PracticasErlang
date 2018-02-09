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
            view_contacts(),
            continue_msg();
        '2' ->
            add_info_prompt(),
            continue_msg();
        '3' ->
            check_contact_prompt(),
            continue_msg();
        '0' ->
            ok;
        '7' ->
            connect_node(),
            continue_msg();
        '8' ->
            peers_info(),
            continue_msg();
        '9' ->
            create_graph(),
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
    io:format("1. Ver contactos~n"),
    io:format("2. Añadir información a contacto~n"),
    io:format("3. Ver Contacto~n"),
    io:format("4. Borrar información de contacto~n"),
    io:format("5. Borrar contacto~n"),
    io:format("7. Conectarse a un nodo~n"),
    io:format("8. Ver nodos conectados~n"),
    io:format("9. Ver grafo de conexiones~n"),
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
      fun ({Server,Status}) -> 
              case Status of 
                  alive -> io:format("El servidor ~w está ONLINE~n",[Server]);
                  dead  -> io:format("El servidor ~w está OFFLINE~n",[Server])
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
        [{_Node,true}|_] -> 
            %io:format("El contacto ya extiste en el nodo ~w, creando contacto~n",[Node]),
            table:add_contact(Id),
            add_info_field(Id);
        []  -> 
            io:format("No existe el contacto~n¿Crear contacto ~w?(s/n)",[Id]),
            {ok,[Choice]} =  io:fread(": ", "~a"),
            case Choice of
                s  -> 
                    table:add_contact(Id),
                    add_info_field(Id);
                _ -> ok
            end
    end.

add_info_field(Id) -> 
    {ok,[Fieldname]} =  io:fread("Nombre del campo: ", "~a"),
    case(check_field(Id,Fieldname)) of
        ok -> add_info_new_field(Id,Fieldname);
        {ok,single}-> add_single_field(Id,Fieldname);
        {ok,multi}->  add_multi_field(Id,Fieldname);
        {Nodename,single} -> add_single_field(Id,Fieldname,Nodename);
        {Nodename,multi}-> add_multi_field(Id,Fieldname,Nodename);
        cancel -> ok
    end.

add_info_new_field(Id,Fieldname)->
    header_msg("Añadir un campo individual o múltiple"),
    io:format("1. Individual (Ej. Nombre, Apellidos, DNI)~n"),
    io:format("2. Múltiple (Ej. Teléfonos, Correos, Direcciones)~n"),
    {ok,[Choice]} =  io:fread("Elección: ", "~a"),
    case Choice of 
        '1'  -> 
            add_single_field(Id,Fieldname);
        '2' ->
            add_multi_field(Id,Fieldname);
        _ -> 
            ok
    end.

add_single_field(Id,Fieldname)  -> 
    io:format("Añadir un campo individual '~w' al contacto '~w'~n",[Fieldname,Id]),
    Value =  getline("Valor del campo: "),
    table:overwrite_single_field(Id,Fieldname,Value).

add_single_field(Id,Fieldname, Nodename)  -> 
    io:format("Añadir un campo individual '~w' al contacto '~w' en el nodo ~w~n",[Fieldname,Id,Nodename]),
    Value =  getline("Valor del campo: "),
    table:overwrite_single_field(Id,Fieldname,Value,Nodename).

add_multi_field(Id,Fieldname)  -> 
    io:format("Añadir un campo múltiple '~w' al contacto '~w'~n",[Fieldname,Id]),
    Values = lists:flatten(ask_for_values(Id,Fieldname)),
    case Values of
        []  -> ok;
        _L ->
            table:add_multi_field(Id,Fieldname,Values)
    end.

add_multi_field(Id,Fieldname,Nodename)  -> 
    io:format("Añadir un campo múltiple '~w' al contacto '~w'~n",[Fieldname,Id]),
    Values = ask_for_values(Id,Fieldname,Nodename),%,Nodename),
    case Values of
        [] -> ok;
        _L ->
            table:add_multi_field(Id,Fieldname,Values,Nodename)
    end.

check_field(Id,Field)  -> 
    case table:ask_node(Id,Field) of
         [{Field, Value}] -> 
            case Value of
                [{_,_}|_] ->
                    io:format("El campo '~w' es un campo múltiple en el nodo actual con valores~n",[Field]),
                    format_info(Value,""),
                    {ok,[Choice]} =  io:fread("¿Desea añadir valores en ese nodo?(s/n): ", "~a"),
                    case Choice of 
                        s  -> 
                            {ok,multi};
                        _ ->
                            cancel
                    end;

                Single_local_value ->
                    io:format("El campo '~w' tiene el valor '~s' en el nodo actual~n",[Field,Single_local_value]),
                    {ok,[Choice]} =  io:fread("¿Desea sobreescribirlo?(s/n): ", "~a"),
                    case Choice of 
                        s  -> 
                            {ok,single};
                        _ ->
                            cancel
                    end
            end;
        [] -> 
            Peers_info = table:ask_network(Id, Field),
            case lists:filter(
                   fun({_, Responses}) ->
                           case Responses of 
                               []  -> false;
                               _  -> true
                           end
                   end,
                   Peers_info) 
            of
                []  -> ok;
                L  ->
                    io:format("Recibido ~w~n",[L]),
                    [{Nodename,[{Field,Dvalue}]}] = L,
                    case Dvalue of 
                        [{_Subfield,_Subvalue}|_]->
                            io:format("El campo '~w' es un campo múltiple en el nodo ~w con valores~n",[Field,Nodename]),
                            format_info(Dvalue,""),
                            {ok,[Choice]} =  io:fread("¿Desea añadir valores en ese nodo?(s/n): ", "~a"),
                            case Choice of 
                                s  -> 
                                    {Nodename,multi};
                                _ ->
                                    cancel
                            end;
                        
                        Singlevalue ->
                            io:format("El campo '~w' es un campo individual con valor '~s' en el nodo ~w~n",[Field,Singlevalue,Nodename]),
                            {ok,[Choice]} =  io:fread("¿Desea sobreescribir el dato en ese nodo?(s/n): ", "~a"),
                            case Choice of 
                                s  -> 
                                    {Nodename,single};
                                _ ->
                                    cancel
                            end
                    end
            end
    end.

format_subfield(Values)->
    lists:foreach(
      fun({_Sf,Sv})->
              io:format("-- ~s~n",[Sv]) 
      end,
      Values).

check_subfield(Id, Field, Subfield) ->
    Ask =table:ask_node(Id,Field,Subfield),
    case Ask of
        [{Subfield, _V}|_] -> 
            io:format("El campo '~w' tiene valores:~n",[Subfield]),
            format_subfield(Ask),    
            io:format("El sistema no permite subcampos con la misma clave y valor~n"),
            {ok,[Choice]} =  io:fread("¿Desea continuar?(s/n): ", "~a"),
            case Choice of 
                s  -> 
                    ok;
                _ ->
                    cancel
            end;
        [] -> ok
    end.

check_subfield(Id, Field, Subfield,Nodename) ->
    Ask =table:ask_node(Id,Field,Subfield,Nodename),
    case Ask of
        [{Subfield, _V}|_] -> 
            io:format("El campo '~w' tiene valores:~n",[Subfield]),
            format_subfield(Ask),    
            io:format("El sistema no permite subcampos con la misma clave y valor~n"),
            {ok,[Choice]} =  io:fread("¿Desea continuar?(s/n): ", "~a"),
            case Choice of 
                s  -> 
                    ok;
                _ ->
                    cancel
            end;
        [] -> ok
    end.

getline(Text)  -> 
    Line = io:get_line(Text),
    string:trim(Line,trailing,"\n").

ask_for_values(Id,Fieldname) ->
    {ok,[Subfieldname]} =  io:fread("Nombre del subcampo: ", "~a"),
    case check_subfield(Id,Fieldname, Subfieldname) of
        ok ->
            Subfieldvalue = getline("Valor del subcampo: "),
            {ok,[Choice]} =  io:fread("¿Añadir otro valor más? (s/n): ", "~a"),
            case Choice of 
                's'  -> [{Subfieldname,Subfieldvalue}|ask_for_values(Id,Fieldname)];
                _ ->  [{Subfieldname,Subfieldvalue}]
            end;
        cancel ->
            {ok,[Choice]} =  io:fread("¿Añadir otro valor más? (s/n): ", "~a"),
            case Choice of 
                's'  -> ask_for_values(Id,Fieldname);
                _ ->  []
            end
    end.

ask_for_values(Id,Fieldname,Nodename) ->
    {ok,[Subfieldname]} =  io:fread("Nombre del subcampo: ", "~a"),
    case check_subfield(Id,Fieldname, Subfieldname,Nodename) of
        ok ->
            Subfieldvalue = getline("Valor del subcampo: "),
            {ok,[Choice]} =  io:fread("¿Añadir otro valor más? (s/n): ", "~a"),
            case Choice of 
                's'  -> [{Subfieldname,Subfieldvalue}|ask_for_values(Id,Fieldname,Nodename)];
                _ ->  [{Subfieldname,Subfieldvalue}]
            end;
        cancel ->
            {ok,[Choice]} =  io:fread("¿Añadir otro valor más? (s/n): ", "~a"),
            case Choice of 
                's'  -> ask_for_values(Id,Fieldname,Nodename);
                _ ->  []
            end
    end.


check_contact_prompt() ->
    title_msg("VER INFORMACIÓN DE CONTACTO"),
        {ok, [Id]} = io:fread("Identificador de contacto: ", "~a"),
    format(table:ask(Id)).
        

create_graph()->
    title_msg("GRAFO DE CONEXIÓN DE LA RED"),
    table:graph().

connect_node()->
    {ok, [Node]} = io:fread("Dirección del nodo (sin comillas): ", "~a"),
    case daemon:connect(Node) of
        {error, Msg}->
            io:format("Error al conectar al nodo ~w~nCausa: ~s~n",[Node,Msg]);
        ok ->
            io:format("Conectado al nodo ~w~n",[Node])
    end.

view_contacts()->
    title_msg("LISTA DE CONTACTOS EN TODOS LOS NODOS"),
    lists:foreach(fun(X)->io:format(" - ~s~n",[X]) end,table:contacts()).
