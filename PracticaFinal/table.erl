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
         exists_contact_node/1,
         exists_contact_network/1,
         contacts/0,
         add_single_field/3,
         overwrite_single_field/3,
         overwrite_single_field/4,
         add_multi_field/3,
         add_multi_field/4,
         graph/0,
         del/1,
         del/2,
         ask/1,
         ask/2,
         ask/3,
         ask_node/2,
         ask_network/2,
         ask_node/3,
         ask_rnode/3,
         ask_rnode/4,
         add_contact/1]).

-behaviour(gen_server).

-include("config.hrl").

init(_)->Tableref=ets:new(data,[set]),
         io:format("Starting table server~n"),
         {ok, {Tableref,""}}.

handle_call(stop_table, _From, State) ->
    io:format("Table server stopping...~n", []), 
    {stop, normal, ok, State};

handle_call({exists_contact,Id}, _From, {Table,Lastreq}) ->
    case  ets:lookup(Table,Id) of
        []  ->
            {reply,false,{Table,Lastreq}};
        [{Id,_Reference}|_] -> 
            {reply,true,{Table,Lastreq}}
    end;

handle_call({exists_contact,Id,Nodes_visited,Callhash}, _From, {Table,Lastreq}) ->
    case Callhash == Lastreq of
        true  -> {reply,[],{Table,Lastreq}};
        false  -> 
           Data = case  ets:lookup(Table,Id) of
                []  ->
                    false;
                [{_Id,_Reference}|_] -> 
                    true
            end,
            Target_list = get_next_calls(Nodes_visited),
            Response = distributed_call(
                         {exists_contact,
                          Id,
                          Nodes_visited++Target_list,
                          Callhash}, 
                         Target_list),
            Response_flattened = lists:flatten(Response),
            Response_result = lists:foldl(fun(X,Y) -> X or Y end,false, Response_flattened),
            {reply,Data or Response_result,{Table,Callhash}}
    end;

handle_call({add_contact,Id}, _From, {Table,Lastreq}) ->
    case  ets:lookup(Table,Id) of
        []  -> ContactEts = ets:new(id,[set]),
               ets:insert(Table,{Id,ContactEts}),
               {reply,{ok,Id},{Table,Lastreq}};
        [{Id, Reference}|_] ->  {reply,{existing,Reference},{Table,Lastreq}}
    end;

handle_call({add_single_field,Id,Field,Value}, _From, {Table,Lastreq}) ->
    case  ets:lookup(Table,Id) of
        []  -> 
            {reply,{error,"User does not exist"},{Table,Lastreq}};
        [{Id, Reference}|_] ->
            case ets:lookup(Reference,Field) of
                []  ->
                    ets:insert(Reference,{Field,Value}),
                    {reply,{ok ,{Id,Field,Value}},{Table,Lastreq}};
                [{_,Existing_value}] ->
                    {reply,{existing,{Existing_value}},{Table,Lastreq}}
            end
    end;

handle_call({overwrite_single_field,Id,Field,Value}, _From, {Table,Lastreq}) ->
    case  ets:lookup(Table,Id) of
        []  -> 
            {reply,{error,"User does not exist"},{Table,Lastreq}};
        [{Id, Reference}|_] ->
            ets:insert(Reference,{Field,Value}),
            {reply,{ok ,{Id,Field,Value}},{Table,Lastreq}}
    end;

handle_call({add_multi_field,Id,Field,Values}, _From, {Table,Lastreq}) ->
    case  ets:lookup(Table,Id) of
        []  -> 
            {reply,{error,"User does not exist"},{Table,Lastreq}};
        [{Id, Reference}|_] ->
            case ets:lookup(Reference,Field) of
                []  ->
                    Name = list_to_atom(atom_to_list(Id)++atom_to_list(Field)),
                    Field_reference = ets:new(Name,[bag]),
                    ets:insert(Reference,{Field,multi,Field_reference}),
                    ets:insert(Field_reference,Values),
                    {reply,{ok ,{Id,Field,Values}},{Table,Lastreq}};
                [{_,multi,Subfield_table}] ->
                    ets:insert(Subfield_table,Values),
                    {reply,{ok,{Id,Field,Values}},{Table,Lastreq}}
            end
    end;


handle_call({del_contact,Id,Nodes_visited,Callhash}, _From, {Table,Lastreq}) ->
    case Callhash == Lastreq of
        true  -> {reply,[],{Table,Lastreq}};
        false  -> 
            Target_list = get_next_calls(Nodes_visited),
            distributed_call({del_contact, Id,
                              Nodes_visited++Target_list,
                              Callhash}, Target_list),
            case ets:lookup(Table,Id) of
                []  -> ok;
                [{Id, Reference}|_] ->
                    ets:delete(Reference),
                    ets:delete(Table,Id)
            end,
            {reply,ok,{Table,Callhash}}
    end;

handle_call({del_field,Id,Field}, _From, {Table,Lastreq}) ->

    case  ets:lookup(Table,Id) of
        []  -> 
            {reply,{error,"User does not exist"},{Table,Lastreq}};
        [{Id, Reference}|_] ->
            case ets:lookup(Reference,Field) of
                []  ->
                    {reply,{error,{Field,"Not found"}},{Table,Lastreq}};
                [{Field,_Value}] ->
                    ets:delete(Reference,Field),
                    {reply,{ok,{Id,Field}},{Table,Lastreq}};
                [{Field,multi,Field_reference}] ->
                    ets:delete(Field_reference),
                    ets:delete(Reference,Field),
                    {reply,{ok,{Id,Field}},{Table,Lastreq}}
            end
    end;

handle_call({del_subfield,Id,Field,Subfield}, _From, {Table,Lastreq}) ->

    case  ets:lookup(Table,Id) of
        []  -> 
            {reply,{error,"User does not exist"},{Table,Lastreq}};
        [{Id, Reference}|_] ->
            case ets:lookup(Reference,Field) of
                []  ->
                    {reply,{error,{Field,"Not found"}},{Table,Lastreq}};
                [{Field,_Value}] ->
                    {reply,{error,{Field,"No es multivalor"}},{Table,Lastreq}};
                [{Field,multi,Field_reference}] ->
                    case(ets:lookup(Field_reference,Subfield)) of
                        []->
                            {reply,{error,{Subfield,"Not found"}},{Table,Lastreq}};
                        _->
                            ets:delete(Field_reference,Subfield),
                            {reply,{ok,{Id,Field,Subfield}},{Table,Lastreq}}
                        end
            end
    end;

handle_call({ask,Id,Field}, _From, {Table,Lastreq}) ->
    Response = get_data(Id,Table,Field),
    {reply,Response,{Table,Lastreq}};

handle_call({ask,Id,Field,Subfield}, _From, {Table,Lastreq}) ->

    case  ets:lookup(Table,Id) of
        []  -> Response = [];
        [{Id, Contact_reference}|_]  -> 
            case ets:lookup(Contact_reference,Field) of
                []  -> Response = [];
                [{Field,multi,Field_reference}] -> Response = ets:lookup(Field_reference,Subfield)
            end
    end,
    {reply,Response,{Table,Lastreq}};


handle_call({dask,Nodes_visited,Id,Callhash}, _From, {Table,Lastreq}) ->
    case Callhash == Lastreq of
        true  -> {reply,[],{Table,Lastreq}};
        false  -> 
            Localdata = get_data(Id,Table),
            Target_list = get_next_calls(Nodes_visited),
            Response = distributed_call(
                         {dask,
                          Nodes_visited++Target_list,
                          Id,Callhash}, Target_list),
            {reply,lists:flatten([{node(),Localdata}| Response]),{Table,Callhash}}
    end;

handle_call({dask,Nodes_visited,Id,Field,Callhash}, _From, {Table,Lastreq}) ->
    case Callhash == Lastreq of
        true  -> {reply,[],{Table,Lastreq}};
        false  -> 
            Localdata = get_data(Id,Table,Field),
            Target_list = get_next_calls(Nodes_visited),
            Response = distributed_call(
                         {dask,
                          Nodes_visited++Target_list,
                          Id,Field,Callhash}, Target_list),
            {reply,lists:flatten([{node(),Localdata}| Response]),{Table,Callhash}}
    end;

handle_call({dask,Nodes_visited,Id,Field,Subfield,Callhash}, _From, {Table,Lastreq}) ->
    case Callhash == Lastreq of
        true  -> {reply,[],{Table,Lastreq}};
        false  -> 
            Localdata = get_data(Id,Table,Field,Subfield),
            Target_list = get_next_calls(Nodes_visited),
            Response = distributed_call(
                         {dask,
                          Nodes_visited++Target_list,
                          Id,Field,Subfield,Callhash}, Target_list),
            {reply,lists:flatten([{node(),Localdata}| Response]),{Table,Callhash}}
    end;

handle_call({digraph,Nodes_visited,Callhash}, _From, {Table,Lastreq}) ->
    case Callhash == Lastreq of
        true  -> {reply,[],{Table,Lastreq}};
        false  -> 
            Target_list = get_next_calls(Nodes_visited),
            Node_rep = 
                lists:map(
                  fun(Nextnodes) -> 
                          "\"" ++ atom_to_list(node()) ++ "\" -- \"" ++
                              atom_to_list(Nextnodes) ++ "\"\n"
                  end,Target_list),
            Response = distributed_call(
                         {digraph,
                          Nodes_visited++Target_list,
                          Callhash}, 
                         Target_list),
            {reply,[Node_rep | Response],{Table,Callhash}}
    end;

handle_call({dcontacts,Nodes_visited,Callhash}, _From, {Table,Lastreq}) ->
    case Callhash == Lastreq of
        true  -> {reply,[],{Table,Lastreq}};
        false  -> 
            Target_list = get_next_calls(Nodes_visited),
            Data =  ets:match_object(Table, {'$1', '_'}),
            Response = distributed_call(
                         {dcontacts,
                          Nodes_visited++Target_list,
                          Callhash}, 
                         Target_list),
            {reply,[Data | Response],{Table,Callhash}}
    end;

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

                                                % ---------------------------------------------
                                                % Server controll
start() -> 
    Host =hd(tl(string:lexemes(atom_to_list(node()),"@"))),
    gen_server:start_link({local, ?SERVERNAME}, ?MODULE, Host, []).

stop() -> 
    call(stop_table).
                                                % --------------------------------------------- 
                                                % Helper Functions

call(Request)  -> 
    gen_server:call(?SERVERNAME, Request).

call(Request,Nodename)  -> 
    gen_server:call({?SERVERNAME,Nodename}, Request).

distributed_call(Request,Nodes)  -> 
    case Nodes of 
        []  ->  [];
        L  -> 
                                                %io:format("Realizando llamada ~w a los nodos ~w~n",[Request,Nodes]),
            lists:map(
              fun (Server) ->
                      gen_server:call({?SERVERNAME,Server},Request)
              end, 
              L
             )
    end.

get_next_calls(Nodes_visited)->
    Neighbors = sets:from_list(daemon:peers_alive()),
    Visited = sets:from_list(Nodes_visited),
    Target = sets:subtract(Neighbors,Visited),
    sets:to_list(Target).

                                                % ---------------------------------------------
                                                % User functions

exists_contact_node(Id)-> 
    call({exists_contact,Id}).

exists_contact_network(Id)-> 
    call({exists_contact,Id,[node()],hash(exists,Id)}).

add_contact(Id)  -> 
    call({add_contact,Id}).

add_single_field(Id, Field, Value)  -> 
    call({add_single_field,Id,Field,Value}).

overwrite_single_field(Id, Field, Value)  -> 
    call({overwrite_single_field,Id,Field,Value}).

overwrite_single_field(Id, Field, Value, Nodename)  -> 
    call({overwrite_single_field,Id,Field,Value},Nodename).

add_multi_field(Id, Field, Value)  -> 
    call({add_multi_field,Id,Field,Value}).

add_multi_field(Id, Field, Value,Nodename)  -> 
    call({add_multi_field,Id,Field,Value},Nodename).

del({Id}) ->
    call({del_contact,Id,[node()],hash(del,Id)});

del({Id,Field})->
    call({del_field,Id,Field});

del({Id,Field,Subfield})->
    call({del_subfield,Id,Field,Subfield}).

del({Id,Field},Nodename)->
    call({del_field,Id,Field},Nodename);

del({Id,Field,Subfield},Nodename)->
    call({del_subfield,Id,Field,Subfield},Nodename).

ask(Id) -> 
    call({dask,[node()],Id,hash(ask,Id)}).

ask(Id, Field) -> 
    Ask= {node(),call({ask,Id,Field})},
    Result =  distributed_call({ask,Id,Field},[]),
    [Ask|Result].

ask(Id,Field,Subfield)->
    call({dask,[node()],Id,Field,Subfield,hash(asksf,Id)}).

ask_node(Id, Field) -> 
    call({ask,Id,Field}).

ask_network(Id,Field) -> 
    call({dask,[node()],Id,Field,hash(askn,Id)}).

ask_node(Id,Field,Subfield) -> 
    call({ask,Id,Field,Subfield}).

ask_rnode(Id, Field,Nodename) -> 
    call({ask,Id,Field},Nodename).

ask_rnode(Id,Field,Subfield,Nodename) -> 
    call({ask,Id,Field,Subfield},Nodename).

hash(Atom,Id)  -> 
    Ask = atom_to_list(Id)++atom_to_list(Atom),
    Now = integer_to_list(erlang:system_time()),
    Node = atom_to_list(node()),
    crypto:hash(md4,Ask++Now++Node).


graph() -> 
    Time = timer(),
    Res = lists:flatten(call({digraph,[node()],hash(graph,network)})),
    io:format("~nLatencia total de la red: ~w ms~n",[get_timer(Time)]),
    io:format("~nGrafo:~n~ngraph g{~n~s}~n",[Res]),
    io:format("Ver representación del grafo en http://www.webgraphviz.com/~n").



contacts() -> 
    List = lists:flatten(call({dcontacts,[node()],hash(contacts,network)})),
    Filtered = lists:map(fun({Id,_Ref})->Id end,List),
    Set = sets:from_list(Filtered),
    sets:to_list(Set).

                                                % --------------------------------------------- 
                                                % Data functions

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
            case ets:match_object(Reference,{Field,multi,'$0'}) of
                []  -> Multifields = [];
                L -> Multifields = extract_multifields(L)
            end,
            ets:match_object(Reference, {Field, '$0'})++Multifields
    end.

get_data(Id,Table,Field,Subfield)  -> 
    case  ets:lookup(Table,Id) of
        []  -> [];
        [{Id, Reference}|_]  -> 
            case ets:lookup(Reference,Field) of
                [{Field,multi,Field_reference}] -> 
                    ets:match_object(Field_reference,{Subfield,'$0'});
                _ -> []
            end
    end.

extract_multifields(List)  -> 
    lists:map(
      fun({Field,multi,Reference_fields})  -> 
              {Field,
               ets:match_object(Reference_fields, {'$0', '$1'})}
      end,
      List
     ).

                                        % --------------------------------------------- 
                                                % Time functions
timer() ->
                                                % inicia el cronómetro para contar el tiempo
    os:timestamp().

get_timer(Time) ->
                                                % captura el tiempo desde que se inicia el cronómetro
    timer:now_diff(os:timestamp(), Time) / 1000.
