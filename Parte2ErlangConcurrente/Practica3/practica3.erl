-module (p3).
-export ([my_spawn_captura/3,my_spawn_monitor/3,my_spawn_onexit/3,my_spawn/4,vivo/0]).


set_timer() ->
  % inicia el cronómetro para contar el tiempo
  statistics(wall_clock).

get_timer() ->
  % captura el tiempo desde que se inicia el cronómetro
  {_, Time2} = statistics(wall_clock),
  % devuelve el valor en ms
  Time2.

% probar con :
%     p3:my_spawn_captura(timer,sleep,[1000]).
my_spawn_captura(Mod, Func, Args) ->
  % inicia el timer (ver set_timer())
  set_timer(),
  % activa las señales de salida para captura
  process_flag(trap_exit, true),
  % crea un proceso enlazado
  PID = spawn_link(Mod, Func, Args),
  receive
    % espera la señal de salida del Pid
    {'EXIT', PID, Reason} ->
      % obtiene el tiempo del cronómetro
      Time = get_timer(),
      io:format("Proceso terminado con pid: ~p, y motivo ~p y ha durado ~pms~n",[PID,Reason,Time])
    end.

% probar con :
%     p3:my_spawn_monitor(timer,sleep,[1000]).
my_spawn_monitor(Mod, Func, Args) ->
  % inicia el timer (ver set_timer())
  set_timer(),
  % lanza monitor para el proceso Pid
  {Pid,MonRef} = spawn_monitor(Mod,Func, Args),
  receive
    % espera las posibles señales del monitor
    % si termina de manera normal
    {'DOWN',MonRef,process,Pid,normal} ->
      % obtiene el tiempo del cronómetro
      Time = get_timer(),
      io:format("Proceso terminado normalmente con duración ~pms~n",[Time]);

    % si termia con motivo Reason
    {'DOWN',MonRef,process,Pid,Reason} ->
      % obtiene el tiempo del cronómetro
      Time = get_timer(),
      io:format("Proceso terminado con pid: ~p, y motivo ~p y ha durado ~pms~n",[Pid,Reason,Time])
end.

% recibe una razón, obtiene el timer y la imprime por pantalla
on_exit_msg(Why) ->
  % obtiene el tiempo del cronómetro
  Time = get_timer(),
  io:format("Proceso terminado con motivo ~p y ha durado ~pms~n",[Why,Time]).


on_exit(Pid, Fun) ->
  spawn(fun() ->
    % se crea un monitor
    Ref = monitor(process, Pid),
    % y se queda a la espera de señales
    receive
      {'DOWN', Ref, process, Pid, Why} ->
        Fun(Why)
    end
  end).

% probar con :
%     p3:my_spawn_onexit(timer,sleep,[1000]).
my_spawn_onexit(Mod, Func, Args) ->
  % inicia el timer (ver set_timer())
  set_timer(),
  % crea el proceso con Pid
  Pid = spawn(Mod, Func, Args),
  % lanza un monitor con una función que
  % imprime un mensaje por pantalla
  % ver on_exit_msg(Why)
  on_exit( Pid,
    fun(Why) ->
      on_exit_msg(Why)
    end).

% probar con :
%     p3:my_spawn(timer,sleep,[3000],2900).
%     p3:my_spawn(timer,sleep,[3000],3100).

my_spawn(Mod, Func, Args, Timer) ->
  % inicia el timer (ver set_timer())
  set_timer(),

  % lanza un proceso con un monitor para controlar si termina correctamente
  {Pid,MonRef} = spawn_monitor(Mod,Func, Args),
  receive
    % espera las posibles señales del monitor
    % si termina de manera normal
    {'DOWN',MonRef,process,Pid,normal} ->
      % obtiene el tiempo del cronómetro
      Time = get_timer(),
      io:format("Proceso terminado normalmente con duración ~pms~n",[Time]);
    {'DOWN',MonRef,process,Pid,Reason} ->
      % obtiene el tiempo del cronómetro
      Time = get_timer(),
      io:format("Proceso terminado con pid: ~p, y motivo ~p y ha durado ~pms~n",[Pid,Reason,Time])

  % despues de un tiempo Timer, mata el proceso enviando una señal 'kill'
  after Timer ->
    io:format("Proceso con pid: ~p, ha durado mas de ~pms... matando proceso~n",[Pid,Timer]),
    exit(Pid, kill)
  end.



keep_alive(Name, Fun) ->
  % inicia el timer (ver set_timer())
  set_timer(),
  % registra el nombre Name
  register(Name, Pid = spawn(Fun)),
  % registra un monitor para el proceso
  on_exit(Pid,
    fun(Why) ->
      % cuando el proceso muere, escribe por pantalla
      % el tiempo que lleva vivo y relanza keep_alive
      on_exit_msg(Why),
      keep_alive(Name, Fun)
    end).

% función que escribe "Estoy vivo" cada cinco segundos
vivo_msg() ->
  receive
  after 5000 ->
    io:format("Estoy vivo~n"),vivo_msg()
  end.

% lanza un keep_alive que escribe un mensaje cada cinco segundos
% si muere el proceso con el comando 'exit(whereis(vivo),kill).'
% escribe por pantalla el tiempo que lleva vivo y lo relanza
% probar con
%    p3:vivo().
%    exit(whereis(vivo),kill).
vivo() ->
  keep_alive(vivo,fun() -> vivo_msg() end).
