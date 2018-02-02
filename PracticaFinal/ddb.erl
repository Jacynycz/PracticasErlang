-module(ddb).
-export([start/0,stop/0,peers_info/0,add/1,ask/1]).

start()  -> io:format("Starting distributed database...~n"),
            io:format("Loading main module...~n"),
            code:ensure_loaded(ddb),
            Modules_list=[daemon,table],
            lists:foreach(fun(X) -> try_to_load(X) end,Modules_list),
            io:format("Connecting to the peer to peer network...~n"),
            connect().

try_to_load(Module)  -> io:format("Loading module ~w...~n",[Module]),
                        case code:ensure_loaded(Module) of
                            {error,_} -> compile:file(Module);
                            _ -> ok
                        end.

connect() -> daemon:start(),
             table:start(),connected.

stop()  -> daemon:stop(),
           table:stop().

                                                %ask(Query)  -> lists:map(fun(X)  -> safecall({table,X},{info,Query}) end,nodes()). 

peers_info() -> Info = daemon:peers_info(),
                io:format("-----------------~n"),
                io:format("PEERS~n"),
                io:format("-----------------~n"),
                lists:foreach(
                  fun ({Status,Server}) -> 
                          case Status of 
                              1 -> io:format("El servidor ~w está ONLINE~n",[Server]);
                              0  -> io:format("El servidor ~w está OFFLINE~n",[Server])
                          end
                  end
                             , Info
                 ),
                io:format("-----------------~n").


add(Tuple)  -> table:insert(Tuple).
ask(Id)  -> Ask = table:ask(Id),
            Info = daemon:peers_info(),
            Result = lists:filtermap(
              fun ({Status,Server}) ->
                      case Status of 
                          1  -> {true, gen_server:call({table,Server},{ask,Id})}; 
                          0 -> false
                      end 
              end, 
              Info
             ), 
            Ask++lists:merge(Result).
