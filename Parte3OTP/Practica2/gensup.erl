
-module(gensup).
-behaviour(supervisor).

-export([start_tree/1,init/1]).

                                                % inciamos el server con un número N de subniveles de supervisores
start_tree(N)  -> supervisor:start_link(?MODULE,N).

init(0)  ->      
% creamos los nombres de los hijos para que no sean iguales
% para ello utilizamos el PID del padre 
    ChildLeftName = list_to_atom("hojaiz"++pid_to_list(self())),
    ChildRightName = list_to_atom("hojader"++pid_to_list(self())),
    {ok,
     {{one_for_one, 20, 10},
      [
       % si N es igual a cero, creamos los hijos
       {ChildLeftName, {exploder, iniciar, []},
        permanent, infinity, worker, dynamic},
       {ChildRightName, {exploder, iniciar, []},
        permanent, infinity, worker, dynamic}
      ]
     }
    };
init(N) ->  
 % creamos los nombres de los hijos para que no sean iguales
 % para ello utilizamos el PID del padre 
    ChildLeftName = list_to_atom("hijoiz"++pid_to_list(self())),
    ChildRightName = list_to_atom("hijoder"++pid_to_list(self())),
    
{ok,
     {{one_for_one, 20, 10},
      [
       % Si N es mayor que cero, llamamos recursivamente a más supervisores
       {ChildLeftName, {gensup, start_tree, [N-1]},
        permanent, infinity, supervisor, dynamic},
       {ChildRightName, {gensup, start_tree, [N-1]},
        permanent, infinity, supervisor, dynamic}
      ]
     }
    }.
