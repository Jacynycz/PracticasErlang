-module(exploder).
-export([init/1, terminate/2, handle_call/3,handle_cast/2,iniciar/0,handle_info/2]).
-behaviour(gen_server).

-define(SERVERNAME, ?MODULE).


init(_)  ->io:format("Inicializando proceso exploder~n"), {ok,false,2000}.

handle_info(_R,_S) ->
    io:format("Ejecutando exploder_loop()~n"),
     case rand:uniform(5) of
         1 -> {stop,fin,false};
         _ -> {noreply,false,2000}
    end.

terminate(_A,_B)  -> io:format("Exploder finished.~n").
handle_call(_,_,_)  -> {noreply,ok,false}. 
handle_cast(_,_)  -> {noreply,false}. 
iniciar() -> gen_server:start_link(?MODULE, false, []).
