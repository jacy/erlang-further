%% @author jacy
%% @doc @todo Add description to monitors.


-module(monitors).
-compile(export_all).
-import(myio,[p/1,p/2]).

%% Monitors are a special type of link, with two differences:
%% 1. They are unidirectional.
%% 2. You can have many of them between two processes


