-module(comp).
-export([compile/0,reload/0]).
-include("config.hrl").

compile() -> io:format("Compiling modules ~w~n",[?MODULES_LIST]),
             lists:map(fun(X) -> compile_module(X) end,?MODULES_LIST).

compile_module(Module_name) -> compile:file(Module_name, [verbose,report_errors,report_warnings]).


reload() -> ok.
