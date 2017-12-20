-module(reloj_server).
-export([reloj/1]).

reloj(T) ->
  receive stop -> ok
  after T ->
    W=time(),
    io:format("Hora: ~p~n",[W]),
    reloj(T)
  end.
