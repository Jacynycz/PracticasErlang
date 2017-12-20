-module(echo_client).
-export([start/0, print/2, stop/1]).

start() -> echo_server:start().

print(P,M) -> P ! {echo, self(), M}, receive E -> E end.

stop(P) -> P ! {stop},ok.
