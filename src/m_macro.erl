%% @author jacy
%% @doc @todo Add description to mMacro.


-module(m_macro).

%% ====================================================================
%% API functions
%% ====================================================================
-compile(export_all).

%% Defining a “function” macro is similar
-define(sub(X,Y), X -Y).

%%There are also a few predefined macros, such as the following:
% • ?MODULE, which is replaced by the current module name as an atom
% • ?FILE, which is replaced by the filename as a string
% • ?LINE, which returns the line number of wherever the macro is placed

macro_function() ->
	io:write(?sub(7, 5)),
'Ending...'.

%% You can also check whether particular macros are defined in your code  
%% and conditionally define other macros based on that result.
-ifdef(DEBUGMODE).
	-define(DEBUG(S), io:format("dbg: "++S)).
	-else.
	-define(DEBUG(S), ok).
-endif.

-ifdef(TEST).
	test_fun() -> io:format("TEST macro is defined.").
-endif.

%% Then, using the compile flags mentioned previously, we can choose whether to define DEBUGMODE
%% as c(Module, [{d,'TEST'},{d,'DEBUGMODE'}])..
check_macro() ->
	?DEBUG("If you can see this, then you should have defined macro DEBUGMODE !"),
'Ending...'.
%% ====================================================================
%% Internal functions
%% ====================================================================


