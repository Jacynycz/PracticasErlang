-module(main).

-behaviour(application).

-export([start/2, stop/1]).

start(normal, Arg) -> gensup:start_tree(Arg).

stop(_) -> ok.