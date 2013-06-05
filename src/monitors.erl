%% @author jacy
%% @doc @todo Add description to monitors.


-module(monitors).
-compile(export_all).
-import(myio,[p/1,p/2]).

%% Monitors are a special type of link, with two differences:
%% 1. They are unidirectional.
%% 2. You can have many of them between two processes (they stack and they have an identity).
%% Links arenât stackable, so the moment you unlink one, you unlink them all and mess up all 
%% the assumptions made by the other libraries. So you need stackable links, and monitors are 
%% your solution, since they can be removed individually

monitor_demo() ->
	%%  erlang:monitor/2, where the first argument is always the atom process and the second one is the pid.
	%% Every time a process you monitor goes down, you will receive such a message, 
	%% in the form {'DOWN', MonitorReference, process, Pid, Reason}. The reference is there to allow you to 
	%% demonitor the process. Remember that moni- tors are stackable, so itâs possible to take more than one down.
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

%%=====================
% Naming Process
%%=====================
naming_process() ->
	spawn(?MODULE, restarter,[]),
	judge("Genesis", "The Lambda Lies Down on Broadway"),
	exit(whereis(critic), solar_storm),
	judge("Rage Against the Turing Machine", "Unit Testify").
	
restarter() ->
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, critic, []),
    register(critic, Pid),
    receive
        {'EXIT', Pid, normal} -> % not a crash
            ok;
        {'EXIT', Pid, shutdown} -> % manual termination, not a crash
            ok;
 		{'EXIT', Pid, _} -> p("1111"),restarter()
	end.

%% process name can be access in different processes, this will lead shared condition problem
%% so we can identify message by reference
judge(Band, Album) ->
    Ref = make_ref(), % Use references (created with make_ref()) as unique values to identify messages 
	p(whereis(critic)),
    critic ! {self(), Ref, {Band, Album}}, % make sure we receive the correct messages from the right process
    receive
        {Ref, Criticism} -> Criticism
    after 2000 ->
		timeout 
	end.

critic() ->
    receive
        {From, Ref, {"Rage Against the Turing Machine", "Unit Testify"}} ->
            From ! {Ref, "They are great!"};
        {From, Ref, {"System of a Downtime", "Memoize"}} ->
            From ! {Ref, "They're not Johnny Crash but they're good."};
        {From, Ref, {"Johnny Crash", "The Token Ring of Fire"}} ->
            From ! {Ref, "Simply incredible."};
        {From, Ref, {_Band, _Album}} ->
            From ! {Ref, "They are terrible!"}
    end,
    critic().



receive_msg() ->
	receive
		{'DOWN', _MonitorReference, process, Pid, Reason} -> io:format("Trap and exit singal from: ~p, reason: ~p~n", [Pid,Reason]);
		Msg -> p("Receive Msg: ", Msg), receive_msg()
	end.
