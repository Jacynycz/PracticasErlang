-module(echo_server).
-export([start/0]).

start() -> spawn(fun()-> loop() end ).

loop() ->
  receive
    {echo, PID ,E} -> PID ! E, loop();
    {stop} -> stop
  end.
