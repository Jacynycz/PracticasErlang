-module(ddb).
-export([init/0,start/1,stop/0,connect/1]).


connect(Ip)  ->
    try_to_load(daemon),
    case is_atom(Ip) of
        true  -> daemon:connect(Ip);
        false -> daemon:connect(list_to_atom(Ip))
    end.

init() -> start(light).

start(Opts)  ->
    check_node(),
    case Opts of
        light  ->
            io:format("Starting light client...~n"), 
            core();
        full  -> 
            io:format("Starting full client...~n"),
            core(),
            interface();
        _ -> 
            io:format("Uso ddb:start(light) para iniciar un nodo sin interfaz~n"),
            io:format("     ddb:start(full) para iniciar un nodo con interfaz~n")
    end.

try_to_load(Module)  -> io:format("Loading module ~w...~n",[Module]),
                        case code:ensure_loaded(Module) of
                            {error,_} -> io:format("*** Error loading module [~w]~n*** Use the comp module to compile all the modules~n",[Module]),throw(module_not_compiled);
                            _ -> ok
                        end.

interface() ->
    interface:start().

core() -> 
    io:format("Starting distributed database...~n"),
    daemon:start(),
    table:start().

stop()  ->
    daemon:stop(),
    table:stop().

check_node()  -> 
    case node() of
        nonode@nohost  -> 
            error("No node connected, start erland with '-name' or '-sname'");
        _ -> ok
    end.

