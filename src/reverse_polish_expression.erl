%% @author jacy
%% @doc @todo Add description to reverse_polish_expression.


-module(reverse_polish_expression).

%% ====================================================================
%% API functions
%% ====================================================================
-compile(export_all).

c(Express) ->
	Tokens = string:tokens(Express, " "),
	
'Ending...'.

rc([],[]) -> 0;
rc([Acc | []],[]) -> Acc;
rc([S_Head | S_Tail],[E_Head | E_Tail]) when (E_Head == "+" or  E_Head == "-" or E_Head =:= "*" or E_Head =:= "/") -> rc([(S_Head + E_Head)| S_Tail], E_Tail);
rc(Stack = [S_Head | S_Tail],[E_Head | E_Tail], Acc) -> rc().



%% ====================================================================
%% Internal functions
%% ====================================================================


