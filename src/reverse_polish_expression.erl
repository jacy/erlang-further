%% @author jacy
%% @doc @todo Add description to reverse_polish_expression.


-module(reverse_polish_expression).

%% ====================================================================
%% API functions
%% ====================================================================
-compile(export_all).
-import(myio,[p/1]).

c(Express) ->
	Tokens = string:tokens(Express, " "),
	%rc([7,"10"],"2 * -").
	rc([],Tokens).

demo() -> p(rc([],[10,4,3,"+",2,"+","+"])).

rc([Acc], []) -> Acc;
rc([H1,H2 | S],["+" | T]) -> rc([(H1 + H2) | S], T);
rc([H1,H2 | S],["-" | T]) -> rc([(H1 - H2) | S], T);
rc([H1,H2 | S],["*" | T]) -> rc([(H1 * H2) | S], T);
rc([H1,H2 | S],["/" | T]) -> rc([(H1 / H2) | S], T);
rc(S,[H | T]) -> rc([H | S], T).


read(N) ->
    case string:to_float(N) of
        {error,no_float} -> list_to_integer(N);
        {F,_} -> F
    end.
%% ====================================================================
%% Internal functions
%% ====================================================================
rpn("+", [N1,N2|S]) -> [N2+N1|S];
rpn(X, Stack) -> [read(X)|Stack].
rpn(L) when is_list(L) ->
    [Res] = lists:foldl(fun rpn/2, [], string:tokens(L, " ")),
    Res.

