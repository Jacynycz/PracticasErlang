-module(ddb).
-export([connect/0,start_daemon/0]).

connect() -> [Nodename|_]=string:lexemes(atom_to_list(node()),"@") ,
             {ok, L} = net_adm:names(), 
             Peername = find_peer(L,Nodename),
             case net_adm:ping(list_to_atom(Peername++"@"++net_adm:localhost())) of
                 pong  -> ok;
                 pang  -> error
             end.

find_peer([],_Nodename) -> error;
find_peer([{Name,_}|Tail],Nodename) -> 
    case Name == Nodename of
        false  -> Name;
        true  -> find_peer(Tail,Nodename)
    end.

start_daemon()  -> daemon:start_daemon().
