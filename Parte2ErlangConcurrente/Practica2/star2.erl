-module(star2).
-export([start/3]).


start(M, N, Msg) -> spawn(fun() -> estrella(M,N,Msg) end).

estrella(M, N, Msg) ->
  L=crea_estrella(N), %crea la estrella y devuelve la lista de nodos
  io:format("creada la estrella con procesos~p~n",[L]),
  envia_mensajes(M,Msg,L) %Manda a la lista de los L nodos, los M mensajes
  .

envia_mensajes(_M,_Msg,[]) -> ok;
envia_mensajes(M,Msg,[Head | Tail]) -> envia_mensaje(M,Msg,Head), envia_mensajes(M,Msg, Tail).

envia_mensaje(0,_Msg,Pid) -> Pid ! {fin};
envia_mensaje(M,Msg,Pid) -> Pid ! {msg,Msg},envia_mensaje(M-1,Msg,Pid) .

crea_estrella(N) -> [spawn_link(fun() -> espera_mensaje(X) end) || X <- lists:seq(1,N)].

espera_mensaje(X) ->
  receive
    {msg,Msg} -> io:format("El proceso número ~p, con PID:~p recibe el mensaje ~p~n",[X,self(),Msg]),espera_mensaje(X);
    {fin} -> io:format("El proceso número ~p, con PID:~p recibe la señal de fin~n",[X,self()])
  end.
