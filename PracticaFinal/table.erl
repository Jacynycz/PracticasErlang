-module(table).

-export([
         init/1, 
         terminate/2, 
         handle_call/3, 
         handle_cast/2,
         handle_info/2,
         code_change/3,
         start/0,
         stop/0,
         exists_contact/1,
         add_single_field/3,
         overwrite_single_field/3,
         add_multi_field/3,
         graph/0,
         ask/1,
         ask/2,
         ask_node/2,
         ask_network/2,
         ask_node/3,
         ask_network/3,
         add_contact/1]).

-behaviour(gen_server).

-include("config.hrl").

init(_)->Tableref=ets:new(data,[set]),
         io:format("Starting table server~n"),
         {ok, Tableref}.

handle_call(stop_table, _From, State) ->
    io:format("Table server stopping...~n", []), 
    {stop, normal, ok, State};

handle_call({exists_contact,Id}, _From, State) ->
    case  ets:lookup(State,Id) of
        []  ->
            {reply,false,State};
        [{Id,_Reference}|_] -> 
            {reply,true,State}
    end;

handle_call({add_contact,Id}, _From, State) ->
    case  ets:lookup(State,Id) of
        []  -> ContactEts = ets:new(id,[set]),
               ets:insert(State,{Id,ContactEts}),
               {reply,{ok,Id},State};
        [{Id, Reference}|_] ->  {reply,{existing,Reference},State}
    end;

handle_call({add_single_field,Id,Field,Value}, _From, State) ->
    case  ets:lookup(State,Id) of
        []  -> 
            {reply,{error,"User does not exist"},State};
        [{Id, Reference}|_] ->
            case ets:lookup(Reference,Field) of
                []  ->
                    ets:insert(Reference,{Field,Value}),
                    {reply,{ok ,{Id,Field,Value}},State};
                [{_,Existing_value}] ->
                    {reply,{existing,{Existing_value}},State}
            end
    end;

handle_call({overwrite_single_field,Id,Field,Value}, _From, State) ->
    case  ets:lookup(State,Id) of
        []  -> 
            {reply,{error,"User does not exist"},State};
        [{Id, Reference}|_] ->
            ets:insert(Reference,{Field,Value}),
            {reply,{ok ,{Id,Field,Value}},State}
    end;

handle_call({add_multi_field,Id,Field,Values}, _From, State) ->
    case  ets:lookup(State,Id) of
        []  -> 
            {reply,{error,"User does not exist"},State};
        [{Id, Reference}|_] ->
            case ets:lookup(Reference,Field) of
                []  ->
                    Name = list_to_atom(atom_to_list(Id)++atom_to_list(Field)),
                    Field_reference = ets:new(Name,[bag]),
                    ets:insert(Reference,{Field,multi,Field_reference}),
                    ets:insert(Field_reference,Values),
                    {reply,{ok ,{Id,Field,Values}},State};
                [{_,Existing_value}] ->
                    {reply,{existing,{Existing_value}},State}
            end
    end;

handle_call({ask,Id}, _From, State) ->
    Response = get_data(Id,State),
    {reply,Response,State};

handle_call({ask,Id,Field}, _From, State) ->
    Response = get_data(Id,State,Field),
    {reply,Response,State};

handle_call({digraph,Nodes_visited}, _From, State) ->
    Neighbors = sets:from_list(daemon:peers_alive()),
    Visited = sets:from_list(Nodes_visited),
    Target = sets:subtract(Neighbors,Visited),
    Target_list = sets:to_list(Target),
    Response = distributed_call(
                 {digraph,
                  Nodes_visited++Target_list
                 }, 
                 Target_list),
    {reply,{node(), Response},State};

handle_call({dask,Nodes_visited,Id}, _From, State) ->
    Localdata = get_data(Id,State),
    io:format("Datos obtenidos~n"),
    Neighbors = sets:from_list(daemon:peers_alive()),
    io:format("Obtenida lista de vecinos vivos ~w~n",[sets:to_list(Neighbors)]),
    Visited = sets:from_list(Nodes_visited),
    io:format("Vecinos visitados ~w~n",[Nodes_visited]),
    Target = sets:subtract(Neighbors,Visited),
    Target_list = sets:to_list(Target),
    io:format("Vecinos a los que realizar petición: ~w~n",[Target_list]),
    Response = distributed_call(
                 {dask,
                  Nodes_visited++Target_list,
                  Id}, Target_list),
    {reply,lists:flatten([{node(),Localdata}| Response]),State};

handle_call({ask,Id,Field,Subfield}, _From, State) ->
    case  ets:lookup(State,Id) of
        []  -> Response = [];
        [{Id, Contact_reference}|_]  -> 
            %ets:lookup(State,Id),
            case ets:lookup(Contact_reference,Field) of
                []  -> Response = [];
                [{Field,multi,Field_reference}] -> Response = ets:lookup(Field_reference,Subfield)
                %L -> Response = L
            end
    end,
    {reply,Response,State};

handle_call(Request, _From, State) -> 
    io:format("Unexpected request: ~w~n", [Request]),
    {noreply, State}.


handle_cast(Request, State) -> 
    io:format("Unexpected request: ~w~n", [Request]),
    {noreply, State}.

handle_info(_Message, State) -> 
    {noreply, State,1000}.

terminate(Reason, _State) -> 
    io:format("Table server finished.~n"),
    io:format("Reason: ~w~n", [Reason]).

code_change(_PreviousVersion, State, _Extra) -> 
    {ok, State}.

call(Request)  -> 
    gen_server:call(?SERVERNAME, Request).

distributed_call(Request,Nodes)  -> 
    case Nodes of 
        []  ->  [];
        L  -> 
            io:format("Realizando llamada ~w a los nodos ~w~n",[Request,Nodes]),
            lists:map(
              fun (Server) ->
                      gen_server:call({?SERVERNAME,Server},Request)
              end, 
              L
             )
    end.

extract_multifields(List)  -> 
    %io:format("Extracting multifields of ~w~n",[L]),
    lists:map(
      fun({Field,multi,Reference_fields})  -> 
              {Field,
               ets:match_object(Reference_fields, {'$0', '$1'})}
      end,
      List
     ).

start() -> 
    Host =hd(tl(string:lexemes(atom_to_list(node()),"@"))),
    gen_server:start_link({local, ?SERVERNAME}, ?MODULE, Host, []).

stop() -> 
    call(stop_table).

exists_contact(Id)  -> 
    case call({exists_contact,Id}) of
        true  -> true;
        false  -> 
            %Other_nodes = 
                lists:filter(
                  fun({_, Bool}) -> Bool end,
                  distributed_call({exists_contact,Id},[])                
                  )
    end.

add_contact(Id)  -> 
    call({add_contact,Id}).

add_single_field(Id, Field, Value)  -> 
    call({add_single_field,Id,Field,Value}).

overwrite_single_field(Id, Field, Value)  -> 
    call({overwrite_single_field,Id,Field,Value}).

add_multi_field(Id, Field, Value)  -> 
    call({add_multi_field,Id,Field,Value}).

ask(Id, Field) -> 
    Ask= {node(),call({ask,Id,Field})},
    Result =  distributed_call({ask,Id,Field},[]),
    [Ask|Result].

ask_node(Id, Field) -> 
    call({ask,Id,Field}).

ask_network(Id,Field) -> 
    distributed_call({dask,Id,Field},[]).

ask(Id) -> 
    %Ask= {node(),call({ask,Id})},
    call({dask,[node()],Id}).
    %[Ask|Result].

graph() -> 
    Time = timer(),
    Res = call({digraph,[node()]}),
    {get_timer(Time),Res}.

ask_node(Id,Field,Subfield) -> 
    call({ask,Id,Field,Subfield}).

ask_network(Id,Field,Subfield) -> 
    distributed_call({ask,Id,Field,Subfield},[]).

get_data(Id,Table) -> 
    case  ets:lookup(Table,Id) of
        []  -> [];
        [{Id, Reference}|_]  ->
            case ets:match_object(Reference,{'$0',multi,'$1'}) of
                []  -> Multifields = [];
                L -> Multifields = extract_multifields(L)
            end,
            ets:match_object(Reference, {'$0', '$1'})++Multifields
    end.

get_data(Id,Table,Field)  -> 
    case  ets:lookup(Table,Id) of
        []  -> [];
        [{Id, Reference}|_]  -> 
                                                %ets:lookup(State,Id),
            case ets:match_object(Reference,{Field,multi,'$0'}) of
                []  -> Multifields = [];
                L -> Multifields = extract_multifields(L)
            end,
            ets:match_object(Reference, {Field, '$0'})++Multifields
    end.

timer() ->
                                                % inicia el cronómetro para contar el tiempo
    os:timestamp().

get_timer(Time) ->
                                                % captura el tiempo desde que se inicia el cronómetro
    timer:now_diff(os:timestamp(), Time) / 1000.
