-module(daemon).

-export([init/1, terminate/2, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3,start/0,start/1,stop/0,peers_info/0,connect/1]).

-behaviour(gen_server).

-include("config.hrl").

init(Host)->io:format("Starting daemon~n"),
            {ok, Host , ?NEIGHBOR_LATENCY}.

handle_call(stop_daemon, _From, State) ->io:format("Daemon server stopping...~n", []), 
                                         {stop, normal, ok, State};

handle_call(ddb_ping, _From, State) -> {reply,1,State,?NEIGHBOR_LATENCY};

handle_call(peers_info, _From,State) -> Nodes = nodes(),
                                        Pings =  lists:map(
                                                   fun(X)  -> safecall({daemon,X},ddb_ping) end,
                                                   Nodes),
                                        {reply,lists:zip(Pings,Nodes),State,?NEIGHBOR_LATENCY}; 

handle_call(Request, _From, State) -> io:format("Unexpected request: ~w~n", [Request]),
                                      {noreply, State,?NEIGHBOR_LATENCY}.

handle_cast(Request, State) -> io:format("Unexpected request: ~w~n", [Request])
                                   ,{noreply, State,?NEIGHBOR_LATENCY}.

handle_info(_Message, State) -> check_peers(State),
                                {noreply, State,?NEIGHBOR_LATENCY}.

terminate(Reason, _State) -> io:format("Daemon server finished.~n"),
                             io:format("Reason: ~w~n", [Reason]).

code_change(_PreviousVersion, State, _Extra) -> {ok, State}.

start(Host) -> gen_server:start_link({local, ?SERVERNAME}, ?MODULE, Host, []).

start() -> 
    Host =hd(tl(string:lexemes(atom_to_list(node()),"@"))),
    gen_server:start_link({local, ?SERVERNAME}, ?MODULE, Host, []).

stop() -> gen_server:call(?SERVERNAME, stop_daemon).

peers_info()  -> gen_server:call(?SERVERNAME,peers_info).

nodename(Node)  -> hd(string:lexemes(atom_to_list(Node),"@")).

check_peers(Host)  ->  {ok, Names} = net_adm:names(),
                       Newpeers = sets:from_list([X || {X,_} <- Names]),
                       Peers = sets:from_list([nodename(X) || X <- nodes()]),
                                                %io:format("Comprobando Peers ~w, ~w~n", [sets:to_list(Peers),sets:to_list(Newpeers)]),
                       case element(2,Peers)+1 == element(2,Newpeers) of
                           true -> ok;% io:format("todo ok~n");
                           false -> actualiza_peers(Newpeers,Peers,Host)
                       end.

actualiza_peers(Newpeers,Peers,Host) ->
    case element(2,Peers)+1  >  element(2,Newpeers) of
        true -> io:format("Desconectados Peers~n");
        false  -> %io:format("Detectados nuevos peers, conectando...~n"),
                  List_of_new_peers = sets:to_list(sets:subtract(Newpeers,Peers)),
                  lists:foreach(fun(X) -> connect_peer(X,Host) end, List_of_new_peers )
    end.

connect_peer(Name,Host)  -> 
    case is_atom(Host) of
        true  -> net_adm:ping(list_to_atom(Name ++"@"++atom_to_list(Host)));
        false ->  net_adm:ping(list_to_atom(Name ++"@"++Host))
    end.


safecall(Call,Query) ->
    try gen_server:call(Call,Query)
    catch _:_  -> 0
    end.


connect(Ip)  -> 
    case net_adm:ping(Ip) of
        pong -> connected;
        pang -> error
    end. 
