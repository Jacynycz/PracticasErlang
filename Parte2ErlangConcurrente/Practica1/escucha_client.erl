-module(escucha_client).
-export([start/0, escucha/2, stop/1]).

start() -> escucha_server:start().

escucha(P,M) -> P ! {self(), M}.

stop(P) -> P ! stop.
