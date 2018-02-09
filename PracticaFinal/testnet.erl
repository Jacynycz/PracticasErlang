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
    compile_and_load(Node1),
    compile_and_load(Node2),
    compile_and_load(Node3),
    compile_and_load(Node4),
    compile_and_load(Node5),
    compile_and_load(Node6),
    connect(Node1,Node2),
    connect(Node2,Node3),
    connect(Node2,Node4),
    connect(Node5,Node3),
    connect(Node5,Node4),
    connect(Node5,Node6).
    

compile_and_load(Node) -> 
    spawn(Node,comp,compile,[]),
    spawn(Node,ddb,init,[]).

connect(Node1,Node2)  -> 
    spawn(Node1,daemon,connect,[Node2]).
