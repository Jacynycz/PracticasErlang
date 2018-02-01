-module(daemon).

-export([init/1, terminate/2, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3,start_daemon/0,end_daemon/0]).

-behaviour(gen_server).

-define(SERVERNAME, ?MODULE).

init(_)->io:format("Starting daemon~n"),
         {ok, daemon_online , 1000}.

handle_call(end_daemon, _From, State) ->io:format("Daemon server ending~n", []), 
                                        {stop, normal, ok, State};
handle_call(Request, _From, State) -> io:format("Unexpected request: ~w~n", [Request]),
                                      {noreply, State}.
handle_cast(Request, State) -> io:format("Unexpected request: ~w~n", [Request])
                                   ,{noreply, State}.

handle_info(_Message, State) -> check_peers(),
                                {noreply, State,1000}.

terminate(Reason, _State) -> io:format("Daemon server finished.~n"),
                             io:format("Reason: ~w~n", [Reason]).

code_change(_PreviousVersion, State, _Extra) -> {ok, State}.

start_daemon() -> gen_server:start_link({local, ?SERVERNAME}, ?MODULE, ok, []).

end_daemon() -> gen_server:call(?SERVERNAME, end_daemon).

nodename(Node)  -> hd(string:lexemes(atom_to_list(Node),"@")).

check_peers()  ->  {ok, Names} = net_adm:names(),
                   Newpeers = sets:from_list([X || {X,_} <- Names]),
                   Peers = sets:from_list([nodename(X) || X <- nodes()]),
                   %io:format("Comprobando Peers ~w, ~w~n", [sets:to_list(Peers),sets:to_list(Newpeers)]),
                   case element(2,Peers)+1 == element(2,Newpeers) of
                       true -> ok;% io:format("todo ok~n");
                       false -> actualiza_peers(Newpeers,Peers)
                   end.

actualiza_peers(Newpeers,Peers) ->
    case element(2,Peers)+1  >  element(2,Newpeers) of
        true -> io:format("Desconectados Peers~n");
        false  -> io:format("Detectados nuevos peers, conectando...~n"),
                  List_of_new_peers = sets:to_list(sets:subtract(Newpeers,Peers)),
                  lists:foreach(fun(X) -> connect_peer(X) end, List_of_new_peers )
    end.

connect_peer(Name)  ->  net_adm:ping(list_to_atom(Name ++"@"++net_adm:localhost())).
