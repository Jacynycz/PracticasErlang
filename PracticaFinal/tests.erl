-module(testnet).
-export([start/0]).

start() -> 
    Localhost = net_adm:localhost(),
    Node1 = list_to_atom("1@"++Localhost),
    Node2 = list_to_atom("2@"++Localhost),
    Node3 = list_to_atom("3@"++Localhost),
    Node4 = list_to_atom("4@"++Localhost),
    Node5 = list_to_atom("5@"++Localhost),
    Node6 = list_to_atom("6@"++Localhost),
    Compileandload =  fun()  -> 
                              c(comp),
                              comp:compile(),
                              ddb:init() 
                      end,
    spawn(Node1,Compileandload),
    spawn(Node2,Compileandload),
    spawn(Node3,Compileandload),
    spawn(Node4,Compileandload),
    spawn(Node5,Compileandload),
    spawn(Node6,Compileandload).

