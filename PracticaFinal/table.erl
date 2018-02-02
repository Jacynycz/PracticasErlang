-module(table).

-export([init/1, terminate/2, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3,start/0,stop/0,insert/1,ask/1]).

-behaviour(gen_server).

-define(SERVERNAME, ?MODULE).

init(_)->Tableref=ets:new(data,[set]),
         io:format("Starting table server~n"),
         {ok, Tableref}.

handle_call(stop_table, _From, State) ->io:format("Table server stopping...~n", []), 
                                        {stop, normal, ok, State};
handle_call({insert,A}, _From, State) -> ets:insert(State,A),
                                         {reply,A,State };
handle_call({ask,A}, _From, State) -> Response = ets:lookup(State,A),
                                         {reply,Response,State };
handle_call(Request, _From, State) -> io:format("Unexpected request: ~w~n", [Request]),
                                      {noreply, State}.
handle_cast(Request, State) -> io:format("Unexpected request: ~w~n", [Request])
                                   ,{noreply, State}.

handle_info(_Message, State) -> {noreply, State,1000}.

terminate(Reason, _State) -> io:format("Table server finished.~n"),
                             io:format("Reason: ~w~n", [Reason]).

code_change(_PreviousVersion, State, _Extra) -> {ok, State}.

start() -> 
    Host =hd(tl(string:lexemes(atom_to_list(node()),"@"))),
    gen_server:start_link({local, ?SERVERNAME}, ?MODULE, Host, []).

stop() -> gen_server:call(?SERVERNAME, stop_table).

insert(Query) -> gen_server:call(?SERVERNAME,{insert,Query}).

ask(Query) -> gen_server:call(?SERVERNAME,{ask,Query}).

