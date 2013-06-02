%% @author jacy
%% @doc @todo Add description to mGaurds.


-module(mguards).
-compile(export_all).
%% ====================================================================
%% API functions
%% ====================================================================

%% In addition to using comparisons and Boolean evaluation in your guards, you can use math operations
%%  (for example, A*B/C >= 0) and functions about data types, such as is_integer/1, is_atom/1, and so on. 
%% One negative point about guards is that they will not accept user-defined functions because of side effects.
%% Erlang is not a purely functional program- ming language (like Haskell) because it relies on side effects a lot.
%% You can do I/O, send messages between actors, or raise exceptions as you want and when you want. 
%% So instead, Erlang just doesn’t trust you (and it may be right not to!).

%% ====================================================================
%% Internal functions
%% ====================================================================


