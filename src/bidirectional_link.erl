%% @author jacy
%% @doc @todo Add description to bidirectional_link.


-module(bidirectional_link).

%% ====================================================================
%% API functions
%% ====================================================================
-compile(export_all).
-import(myio,[p/1]).

demo() ->
	p(self()),
	process_flag(trap_exit,true),
	spawn_link(?MODULE, receive_msg, []),
	p(self()).
	

%% ====================================================================
%% Internal functions
%% ====================================================================
receive_msg() ->
	receive 
		{'EXIT', Pid, Reason} -> io:format("oooooooooooooooooops ~p died because of ~p ooooooooooooooops~n",[Pid, Reason]);
		Msg -> io:format("Receive Msg: ~p ~n ", [Msg]), receive_msg() 
	end.

