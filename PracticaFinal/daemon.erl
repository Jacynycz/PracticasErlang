-module(daemon).

-export([init/1, terminate/2, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3,start/0,stop/0,peers_info/0,peers_alive/0,connect/1]).

-behaviour(gen_server).

-include("config.hrl").

init(_)->io:format("Starting daemon~n"),
         {ok, []}.

handle_call(stop_daemon, _From, State) ->
    io:format("Daemon server stopping...~n", []), 
    {stop, normal, ok, State};

handle_call(daemon_alive, {_,Ref}, State) ->
    Requester = node(Ref),
    io:format("Recibida petición de ~w~n",[Requester]),
    case Requester == node() of
        true  -> 
            ok;
        false  -> 
            case lists:keyfind(Requester,1,State) of
                false  -> 
                    {reply,alive,[{Requester,alive}|State]};
                _  ->
                    {reply,alive,State}
            end 
    end;

handle_call({connect, Server}, _From, State) -> 
                                                %check if server is an atom
    case is_atom(Server) of
        true -> 
                                                % check if it is possible to connect to the node
            case net_adm:ping(Server) of
                pong  -> 
                                                % check if the Node is in the list
                    case lists:keyfind(Server,1,State) of
                        false  -> 
                                                % check if there's a daemon running
                            case  is_alive(Server) of
                                alive  -> 
                                    {reply,ok,[{Server, alive} | State]};
                                dead  -> 
                                    {reply,{error,"El nodo no tiene la base de datos iniciada"},State}
                            end; 
                        _  ->
                            {reply,{error,"Ya conectado"},State}
                    end ;
                pang  -> 
                    {reply,{error, "No se puede conectar"}, State}
           end;
       _ ->
           {reply,{error,"Error en el formato de la dirección "},State}
   end;

handle_call(peers_info, _From,State) ->
    Newstate = check_peers(State),
    {reply,Newstate,Newstate}; 

handle_call(peers_alive, _From,State) ->
    Newstate = check_peers(State),
    Peers = lists:filtermap(
              fun({Dir,Is_alive}) -> 
                      case Is_alive of
                          dead  -> false;
                          alive  -> {true,Dir}
                      end
              end,
              Newstate
             ),
    {reply,Peers,Newstate}; 

handle_call(Request, _From, State) -> 
    io:format("Unexpected request: ~w~n", [Request]),
    {noreply, State}.

handle_cast(Request, State) -> 
    io:format("Unexpected request: ~w~n", [Request])
        ,{noreply, State,?NEIGHBOR_LATENCY}.

handle_info(_Message, State) ->
    {noreply, State}.

terminate(Reason, _State) -> 
    io:format("Daemon server finished.~n"),
    io:format("Reason: ~w~n", [Reason]).

code_change(_PreviousVersion, State, _Extra) -> 
    {ok, State}.


call(Request)  -> 
    gen_server:call(?SERVERNAME, Request).

start() -> 
    gen_server:start_link({local, ?SERVERNAME}, ?MODULE, [], []).

stop() ->
    call(stop_daemon).

peers_info()  ->
    call(peers_info).

peers_alive() -> 
    call(peers_alive).

is_alive(Node) ->
    try gen_server:call({?SERVERNAME,Node},daemon_alive)
    catch _:_  -> dead
    end.

connect(Node)  -> 
    call({connect,Node}).

check_peers(State) -> 
    check_peers_(State).

check_peers_([]) -> [];
check_peers_([{Node,_}| Rest]) -> 
    [{Node,is_alive(Node)}|check_peers_(Rest)].
