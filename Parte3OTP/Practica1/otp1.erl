-module(otp1).
-export([iniciar/0,init/1, terminate/2, handle_call/3,handle_cast/2,
         handle_info/2, code_change/3,nuevo_trabajo/1,trabajo_terminado/1,
        obtener_trabajo/0]).
-behaviour(gen_server).

-define(SERVERNAME, ?MODULE).




init({[],[]}) -> {ok, {[],[]}}.


handle_call(obtener_trabajo, {From,_Tag}, {[Head  | Queue],Work}) ->
    Pid = spawn(Head),
    Ref = erlang:make_ref(),
    io:format("Realizando trabajo con PID ~w y Ref ~w, pedido por ~w~n",[Pid,Ref,From]),
    { reply , { Ref, Head }, { Queue, [ {Ref,Pid,From} | Work ] } };

handle_call(obtener_trabajo, _From, {[],Work}) ->
    io:format("No quedan trabajos disponibles~n"),
    { reply , no , { [], Work } };

handle_call({trabajo_terminado,Ref},{From,_Tag}, {Queue,Work}) ->
    case lists:keyfind(Ref ,1, Work) of
        {Ref, Pid, From} -> 
            io:format("El Pid ~w termina el trabajo con Ref:~w y Pid:~w~n",[From,Ref,Pid]),
            {reply, ok, { Queue , lists:keydelete(Ref,1,Work)}};
         _  ->
            io:format("No se ha encontrado el trabajo con Ref:~w enviado por el Pid:~w~n ",[Ref,From]),
            { reply , error, { Queue ,  Work } }
    end.


handle_cast({nuevo_trabajo, F} , {Queue,Work}) -> 
    io:format("Añadiendo trabajo ~w~n",[F]),
{noreply, {Queue++[F],Work}}.

handle_info(Message, State) -> io:format("Unexpected message: ~w~n", [Message]),
                               {noreply, State}.

terminate(_Reason, _State) -> io:format("Area server finished.~n").

code_change(_PreviousVersion, State, _Extra) -> {ok, State}.

iniciar() -> gen_server:start_link({local, ?SERVERNAME}, ?MODULE, {[],[]}, []).
nuevo_trabajo(F) -> gen_server:cast(?SERVERNAME,{nuevo_trabajo,F}).
obtener_trabajo() -> gen_server:call(?SERVERNAME,obtener_trabajo).
trabajo_terminado(Ref) -> gen_server:call(?SERVERNAME,{trabajo_terminado,Ref}).
