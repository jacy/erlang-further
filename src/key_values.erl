-module(key_values).
-compile(export_all).
-import(myio,[p/1]).


%% ====================================================================
%% For storing small amounts of data, basically two types of data structures can be used:
%% a property list (proplist) or an ordered dictionary (orddict).
%% ====================================================================

%% A proplist is any list of tuples of the form [{Key,Value}].
proplist() ->
	Pl = [{jacy,19}, {lina,18}],
	p(proplists:get_value(jacy, Pl)),
	p(proplists:delete(jacy, Pl)),
	p(proplists:lookup(lina, Pl)),
'end proplist().'.

%%  Ordered dicts are proplists with a taste for formality. Each key can be there only once. 
%% The whole list is sorted so, on average, lookups are faster. 

%% Orddicts are generally a good compromise between complexity and efficiency for up to about 75 elements. 
orddicts() ->
	Od = [{jacy,19}, {lina,18}],
	p(orddict:find(jacy,Od)),
	p(orddict:erase(jacy,Od)),
'end orddicts().'.

%% ====================================================================
%% Erlang provides two key/value struc- tures to deal with larger amounts of data: 
%% dictionaries (dicts) and general balanced (GB) trees.
%% ====================================================================
