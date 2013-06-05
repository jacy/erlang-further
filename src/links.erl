%% @author jacy
%% @doc @todo Add description to links.


-module(links).

%% ====================================================================
%% API functions
%% ====================================================================
-compile(export_all).
-import(myio,[p/1,p/2]).


link_demo() ->
	process_flag(trap_exit,true),
	spawn_link(?MODULE, chain, [3]),
	receive_msg().
	
receive_msg() ->
	receive 
		{'EXIT', Pid, Reason} -> io:format("oooooooooooooooooops ~p died because of ~p ooooooooooooooops~n",[Pid, Reason]);
		Msg -> io:format("Receive Msg: ~p ~n ", [Msg]), receive_msg() 
	end.

chain(0) ->
	  receive
	      _ -> ok
	  after 2000 ->
	      exit("chain dies here") % Exit deliberately.
	  end;

chain(N) ->
      Pid = spawn(fun() -> chain(N-1) end),
      link(Pid), % Link process
      receive
		_ -> ok 
	  end.


%%%Exceptions and Traps
% There’s a load of reasons why processes ususally die. Let’s look at a few of them and what the reasons look like when exits are trapped.
% 1. Exception source: spawn_link(fun() -> ok end)
% 	1) Untrapped result: Nothing
% 	2) Trapped result: {'EXIT', <0.61.0>, normal}
% 	3) The process exited normally, without a problem. Note that this looks a bit like the result of catch exit(normal), except a pid is added 
%		to the tuple to identify which process failed.
% 2. Exception source: spawn_link(fun() -> exit(reason) end)
% 	1) Untrapped result: ** exception exit: reason Trapped result: {'EXIT', <0.55.0>, reason}
% 	2) The process has terminated for a custom reason. If there is no trapped exit, the process crashes. While trapping exits, you get a message.
% 3. Exception source: spawn_link(fun() -> exit(normal) end)
% 	1) Untrapped result: Nothing
% 	2) Trapped result: {'EXIT', <0.58.0>, normal}
% 	3) This successfully emulates a process terminating normally. In some cases, you might want to kill a process as part of the normal flow of a program, 
%		without anything exceptional going on. This is the way to do it.
% 4. Exception source: spawn_link(fun() -> 1/0 end) Untrapped result:
% 	1) Error in process <0.44.0> with exit value: {badarith, [{erlang, '/', [1,0]}]} Trapped result: {'EXIT', <0.52.0>, {badarith, [{erlang, '/', [1,0]}]}}
% 	2) The error ({badarith, Reason}) is never caught by a try ... catch block and bubbles up into an 'EXIT'. At this point, it behaves exactly the same as exit(reason) does, but with a stack trace giving more details about what happened.
% 5. Exception source: spawn_link(fun() -> erlang:error(reason) end) Untrapped result:
% 	1) Error in process <0.47.0> with exit value: {reason, [{erlang, apply, 2}]} Trapped result: {'EXIT', <0.74.0>, {reason, [{erlang, apply, 2}]}}
% 	2) This is pretty much the same as with 1/0. That’s normal—erlang:error/1 is meant to allow you to do just that.
% 6. Exception source: spawn_link(fun() -> throw(rocks) end)
% 	1) Untrapped result:
% 	2) Error in process <0.51.0> with exit value: {{nocatch, rocks}, [{erlang, apply, 2}]}
% 	3) Trapped result: {'EXIT', <0.79.0>, {{nocatch, rocks}, [{erlang, apply, 2}]}}
% 	4) Because the throw is never caught by a try ... catch, it bubbles up into an error, which in turn bubbles up into an EXIT. Without trapping exits, the process fails. While trapping exits, it deals with the error just fine.
