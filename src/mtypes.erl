%% @author jacy
%% @doc @todo Add description to mtypes.


-module(mtypes).

%% ====================================================================
%% API functions
%% ====================================================================
-compile(export_all).
-import(myio,[p/1,p/2]).

boolean() ->
	p(not true),
	p(true or false),
	p(true and false),
	p(not (true and false)),
'Ending...'.
	
equality_test()->
	%% As a general rule of thumb, you should always start by using =:= and =/=, 
	%% and switch to == and /= only when you know you do not need exact equality. 
	p("5 == 5.0 is ", 5 == 5.0),
	p("5 /= 5.0 is ",5 /= 5.0), % key an eye on /=
	p("5 =:= 5.0 is ", 5 =:= 5.0),
	p("5 =/= 5.0 is ",5 =/= 5.0),
	
	p("1 > 5 is ",1 > 5),
	p("1 < 5 is ",1 < 5),
	p("1 >= 5 is ",1 >= 5),
	p("1 =< 5 is ",1 =< 5), % key an eye on =<
'Ending...'.
	

%% ====================================================================
%% Internal functions
%% ====================================================================


