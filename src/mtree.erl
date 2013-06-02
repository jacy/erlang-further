%% @author jacy
%% @doc @todo Add description to mtree.


-module(mtree).

%% ====================================================================
%% API functions
%% ====================================================================
-compile(export_all).

empty() -> nil.

insert(Key, Val, nil) -> {Key, Val, nil, nil};
insert(NewKey, NewVal, {Key, Val, Smaller, Larger}) when NewKey < Key -> {Key, Val, insert(NewKey, NewVal, Smaller), Larger};
insert(NewKey, NewVal, {Key, Val, Smaller, Larger}) when NewKey > Key -> {Key, Val, Smaller, insert(NewKey, NewVal, Larger)};
insert(Key, Val, {Key, _, Smaller, Larger}) -> {Key, Val, Smaller, Larger}.

lookup(_, nil) ->  undefined;
lookup(Key, {Key, Val, _, _}) -> {ok, Val};
lookup(Key,  {NodeKey, _, Smaller, _}) when Key < NodeKey -> lookup(Key, Smaller);
lookup(Key, {_, _, _, Larger}) -> lookup(Key, Larger).

%% ====================================================================
%% Internal functions
%% ====================================================================


