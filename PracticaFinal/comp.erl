-module(comp).
-export([compile/0,reload/0]).
-include("config.hrl").

compile() -> 
    io:format("Compiling modules ~w~n",[?MODULES_LIST]),
    lists:map(fun(X) -> compile_module(X) end,?MODULES_LIST).

compile_module(Module_name) ->
    compile:file(Module_name, [verbose]),reload_module(Module_name).


reload() ->
    lists:map(fun(X) -> reload_module(X) end,?MODULES_LIST).

reload_module(Module_name) -> 
    case code:is_loaded(Module_name) of
        {_,_} ->
            code:purge(Module_name);
        false ->
            ok
    end,
    code:load_file(Module_name).
