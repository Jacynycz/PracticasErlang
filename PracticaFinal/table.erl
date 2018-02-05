-module(table).

-export([init/1, terminate/2, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3,start/0,stop/0,insert/3,ask/1,ask/2]).

-behaviour(gen_server).

-define(SERVERNAME, ?MODULE).

init(_)->Tableref=ets:new(data,[set]),
         io:format("Starting table server~n"),
         {ok, Tableref}.

handle_call(stop_table, _From, State) ->io:format("Table server stopping...~n", []), 
                                        {stop, normal, ok, State};

handle_call({insert,Id,Field,Value}, _From, State) ->
    case  ets:lookup(State,Id) of
        []  -> Newreference = ets:new(id,[set]),
               ets:insert(Newreference,{Field,Value}),
               ets:insert(State,{Id,Newreference});
        [{Id, Reference}|_] -> ets:insert(Reference,{Field,Value})
    end,
                                                %ets:insert(State,A),
    {reply,{Id,Field,Value},State};

handle_call({ask,Id}, _From, State) ->
    case  ets:lookup(State,Id) of
        []  -> Response = "";
        [{Id, Reference}|_]  ->  ets:lookup(State,Id),
                                 Response = ets:match_object(Reference, {'$0', '$1'})
    end,
    {reply,Response,State };

handle_call({ask,Id,Field}, _From, State) ->
    case  ets:lookup(State,Id) of
        []  -> Response = "";
        [{Id, Reference}|_]  ->  ets:lookup(State,Id),
                                 Response = ets:match_object(Reference, {Field, '$1'})
    end,
    {reply,Response,State };

handle_call(Request, _From, State) -> io:format("Unexpected request: ~w~n", [Request]),
                                      {noreply, State}.
handle_cast(Request, State) -> io:format("Unexpected request: ~w~n", [Request])
                                   ,{noreply, State}.

handle_info(_Message, State) -> {noreply, State,1000}.

terminate(Reason, _State) -> io:format("Table server finished.~n"),
                             io:format("Reason: ~w~n", [Reason]).

code_change(_PreviousVersion, State, _Extra) -> {ok, State}.

start() -> 
    Host =hd(tl(string:lexemes(atom_to_list(node()),"@"))),
    gen_server:start_link({local, ?SERVERNAME}, ?MODULE, Host, []).

stop() -> gen_server:call(?SERVERNAME, stop_table).

insert(Id,Field,Value) -> gen_server:call(?SERVERNAME,{insert,Id,Field,Value}).

ask(Query, Field) -> 
    Ask= gen_server:call(?SERVERNAME,{ask,Query,Field}),
    Info = daemon:peers_info(),
    Result = lists:filtermap(
               fun ({Status,Server}) ->
                       case Status of 
                           1  -> {true, gen_server:call({?SERVERNAME,Server},{ask,Query,Field})}; 
                           0 -> false
                       end 
               end, 
               Info
              ), 
    Ask++lists:merge(Result).


ask(Query) -> 
    Ask= gen_server:call(?SERVERNAME,{ask,Query}),
    Info = daemon:peers_info(),
    Result = lists:filtermap(
               fun ({Status,Server}) ->
                       case Status of 
                           1  -> {true, gen_server:call({?SERVERNAME,Server},{ask,Query})}; 
                           0 -> false
                       end 
               end, 
               Info
              ), 
    Ask++lists:merge(Result).

