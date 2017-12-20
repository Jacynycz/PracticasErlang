-module(ring).
-export([start/3]).

start(M, N, Msg) -> Pid=spawn(fun() -> crea_anillo(N, self()) end),envia_menaje(M,Msg,Pid).

envia_menaje(M,Msg,Pid) -> Pid ! {msg,M,Msg}.

crea_anillo(0, PidIni) -> io:format("El proceso último proceso se ha creado~n"),link(PidIni),loop(PidIni);
crea_anillo(N, PidIni) ->
  NextPid = spawn_link(fun() -> crea_anillo(N-1, PidIni) end),
  io:format("El proceso ~p se crea~n",[NextPid]),
  loop(NextPid).

loop(NextPid) ->
  receive
    {msg,0,Msg} -> io:format("El proceso ~p recibe el último mensaje ~p~n",[self(),Msg]),exit("cadena terminada");
    {msg,M,Msg} -> io:format("El proceso ~p le pasa el mensaje a ~p~n",[self(),NextPid]),
    NextPid ! {msg,M-1,Msg},
    loop(NextPid)
  end.
