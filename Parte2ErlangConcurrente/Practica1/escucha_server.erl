-module(escucha_server).
-export([start/0]).

start() ->   io:format("Iniciando server escucha~n"),spawn(fun()-> loop() end ).

loop() ->
  receive
    stop -> io:format("Proceso terminado~n");
    {PID, Msg} -> io:format("~p~n",[Msg]),PID ! ok, loop()
  after 5000 ->
    io:format("Esperando mensajes~n"),loop()
  end.
