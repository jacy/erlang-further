%% @author jacy
%% @doc @todo Add description to monitors.


-module(monitors).
-compile(export_all).
-import(myio,[p/1,p/2]).

%% Monitors are a special type of link, with two differences:
%% 1. They are unidirectional.
%% 2. You can have many of them between two processes (they stack and they have an identity).
%% Links aren’t stackable, so the moment you unlink one, you unlink them all and mess up all 
%% the assumptions made by the other libraries. So you need stackable links, and monitors are 
%% your solution, since they can be removed individually

monitor_demo() ->
	%%  erlang:monitor/2, where the first argument is always the atom process and the second one is the pid.
	%% Every time a process you monitor goes down, you will receive such a message, 
	%% in the form {'DOWN', MonitorReference, process, Pid, Reason}. The reference is there to allow you to 
	%% demonitor the process. Remember that moni- tors are stackable, so it’s possible to take more than one down.
	erlang:monitor(process, spawn(fun() -> timer:sleep(500) end)),
	
	%% Also note that as with links, there is an atomic function to spawn a process while monitoring it: spawn_monitor/1-3.
	receive_msg().

%% Also note that as with links, there is an atomic function to spawn a process while monitoring it: spawn_monitor/1-3.
another_monitor() ->
	{Pid, _} = spawn_monitor(fun() -> receive _ -> exit(boom) end end),
	Pid ! die,
	receive_msg().

unmonitor() ->
	{Pid, Ref} = spawn_monitor(fun() -> receive _ -> exit(boom) end end),
	erlang:demonitor(Ref),
	Pid ! die,
	receive_msg().


receive_msg() ->
	receive
		{'DOWN', _MonitorReference, process, Pid, Reason} -> io:format("Trap and exit singal from: ~p, reason: ~p~n", [Pid,Reason]);
		Msg -> p("Receive Msg: ", Msg), receive_msg()
	end.
