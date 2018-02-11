-module(testnet).
-export([start/0]).

start() -> 
    Localhost = net_adm:localhost(),
    comp:compile(),
    Node1 = list_to_atom("1@"++Localhost),
    Node2 = list_to_atom("2@"++Localhost),
    Node3 = list_to_atom("3@"++Localhost),
    Node4 = list_to_atom("4@"++Localhost),
    Node5 = list_to_atom("5@"++Localhost),
    Node6 = list_to_atom("6@"++Localhost),
    start(Node1),
    start(Node2),
    start(Node3),
    start(Node4),
    start(Node5),
    start(Node6),
    load(Node1,"1.info"),
    load(Node2,"2.info"),
    load(Node3,"3.info"),
    load(Node4,"4.info"),
    load(Node5,"5.info"),
    load(Node6,"6.info"),
    connect(Node1,Node2),
    connect(Node2,Node3),
    connect(Node2,Node4),
    connect(Node5,Node3),
    connect(Node5,Node4),
    connect(Node5,Node6).
    


start(Node) -> 
    spawn(Node,ddb,init,[]).

load(Node,Filename)->
    spawn(Node,loader,load,[Filename]).

connect(Node1,Node2)  -> 
    spawn(Node1,daemon,connect,[Node2]).
