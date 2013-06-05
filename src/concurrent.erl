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

%% Something annoying with the previous example is that the programmer whoâs going to use the fridge must 
%% know about the protocol that has been invented for that process. Thatâs a useless burden. A good way to
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

%% Another special case is when the timeout is at 0 : If there is no matching message in the mailbox, 
%% the timeout will occur immediately.
%% Test it from shell
%% self() ! one.
%% self() ! two.
%% concurrent:flush().
flush() ->
    receive
        Msg -> p(Msg),flush()
    after 0 -> ok 
	end.
%% ====================================================================
%% Selective Receives
%% ====================================================================
important() ->
	receive
	     {Priority, Message} when Priority > 10 -> [Message | important()]
	after 0 ->
	     normal()
	end.

normal() ->
	receive
		{_, Message} -> [Message | normal()]
	after 0 -> []
	end.

%% Handle priority above 10 first:
selective_receives() ->
	self() ! {15, high_15}, 
	self() ! {7, low_7}, 
	self() ! {1, low_1}, 
	self() ! {17, high_17},
	important().
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

%%% When messages are sent to a process, they’re stored in the mailbox until the process reads them and they match a pattern there, 
%%% even if the process that originally sent them has died since then. The messages are stored in the order they were received. 
%%% This means every time you enter a receive to match a message, the mailbox is scanned, beginning with the first (and old- est) message received.

%%% That oldest message is then tried against every pattern of the receive until one of them matches. When it does, the message 
%%% is removed from the mailbox, and the code for the process executes normally until the next receive. When this next receive is executed, 
%%% the VM will look for the oldest message currently in the mailbox (the one after the one you removed), and so on.

%%% When there is no way to match a given message, it is put in a save queue, and the next message is tried. 
%%% If the second message matches, the first mes- sage is put back on top of the mailbox to be retried later.

%%% imagine we want the 367th message, but the first 366 messages are junk ignored by our code. To get the 367th message, 
%%% the process needs to try to match those 366 junk messages. Once it has done that, and the messages have all been put in the queue, 
%%% the 367th message is taken out, and the first 366 are put back on top of the mailbox. The next useful message could be burrowed much 
%%% deeper and take even longer to be found.This kind of receive is a frequent cause of performance problems in Erlang. 
