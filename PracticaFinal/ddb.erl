-module(ddb).
-export([start/0,stop/0]).

start()  ->
    io:format("Starting distributed database...~n"),
    io:format("Loading main module...~n"),
    code:ensure_loaded(ddb),
    Modules_list=[daemon,table],
    lists:foreach(fun(X) -> try_to_load(X) end,Modules_list),
    io:format("Connecting to the peer to peer network...~n"),
    connect().

try_to_load(Module)  -> io:format("Loading module ~w...~n",[Module]),
                        case code:ensure_loaded(Module) of
                            {error,_} -> io:format("*** Error loading module [~w]~n*** Use the comp module to compile all the modules~n",[Module]),throw(module_not_compiled);
                            _ -> ok
                        end.

connect() ->
    daemon:start(),
    table:start(),
    interface:start().

stop()  ->
    daemon:stop(),
    table:stop().



