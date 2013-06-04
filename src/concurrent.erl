%% @author jacy
%% @doc @todo Add description to concurrent.


-module(concurrent).

%% ====================================================================
%% API functions
%% ====================================================================
-compile(export_all).
-import(myio,[p/1,p/2]).


 parallel() ->
	F = fun(Id) ->  timer:sleep(100), p(Id) end,
	[spawn(fun() -> F(X) end) || X <- lists:seq(1, 10)].

shift_state() -> 
	Pid = spawn(?MODULE, fridge, [[]]),
	From = spawn(?MODULE, from, []),
	Pid ! {From, {store, pomegranate}}, % Have to know the message protol
	Pid ! {From, {take, pomegranate}},
	Pid ! {From, {take, pomegranate}},
	Pid ! terminate.

%% Something annoying with the previous example is that the programmer who’s going to use the fridge must 
%% know about the protocol that has been invented for that process. That’s a useless burden. A good way to
%% solve this is to abstract messages away with the help of functions dealing with receiving and sending them:
hide_msg() -> 
	Pid = spawn(?MODULE, fridge, [[]]),
	take(Pid, pomegranate),
	store(Pid, pomegranate),
	take(Pid, pomegranate).

store(Pid, Food) -> 
	Pid ! {self(), {store, Food}}, % Keep Message secret
	 receive
         {Pid, _} -> p("store food success: ", Food)
	 after 3000 -> p("Can not send message to ", Pid),timeout % timeout handle
     end.

take(Pid, Food) -> 
	Pid ! {self(), {take, Food}}, % Keep Message secret
	 receive
         {Pid, ok} -> p("take food success: ", Food);
         {Pid, not_found} -> p("take food Failed: ", Food)
		 after 3000 -> p("Can not send message to ", Pid),timeout % timeout handle
     end.

%select_prior() ->
	
%% ====================================================================
%% System feature implementation source analysis
%% ====================================================================
%% With the help of timeout handler, we can implement sleep feature easily.
sleep(T) ->
    receive % no message will ever be matched in the receive part of the construct because there is no pattern. 
    	after T -> ok
    end.

%% Another special case is when the timeout is at 0:
%% When that happens, the Erlang VM will try to find a message that fits one of the available patterns. 
%% In the preceding case, anything matches. As long as there are messages, the flush/0 function will recursively 
%% call itself until the mailbox is empty. After that, the after 0 -> ok part of the code is executed, 
%% and the function returns.
flush() ->
    receive
        _ -> flush()
    after 0 -> ok 
	end.
%% ====================================================================
%% Internal functions
%% ====================================================================
from() -> receive Msg -> p("receiveMsg from server: ", Msg),from() end.

fridge(List) ->
	receive
		{From,{store, Food}} -> From ! {self(), ok},fridge([Food | List]);
		{From,{take, Food}} -> 
			case lists:member(Food, List) of 
				true -> From ! {self(), ok}, fridge(lists:delete(Food, List));
				false -> From ! {self(), not_found}, fridge(List)
			end;
		 terminate -> ok
	end.
