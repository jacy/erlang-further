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
rc([S_Head | [S_Head_2 | S_Tail]],["+" | E_Tail]) -> rc([(S_Head + S_Head_2) | S_Tail], E_Tail);
rc([S_Head | [S_Head_2 | S_Tail]],["-" | E_Tail]) -> rc([(S_Head - S_Head_2) | S_Tail], E_Tail);
rc([S_Head | [S_Head_2 | S_Tail]],["*" | E_Tail]) -> rc([(S_Head * S_Head_2) | S_Tail], E_Tail);
rc([S_Head | [S_Head_2 | S_Tail]],["/" | E_Tail]) -> rc([(S_Head / S_Head_2) | S_Tail], E_Tail);
rc(Stack,[E_Head | E_Tail]) -> rc([E_Head | Stack], E_Tail).



%% ====================================================================
%% Internal functions
%% ====================================================================


