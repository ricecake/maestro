-module(maestro).

%% API exports
-export([
	ping/0,
	add_timer/2,
	remove_timer/1
]).

%%====================================================================
%% API functions
%%====================================================================

ping() -> maestro_dist:ping().

add_timer(Name, Data) ->
	maestro_dist:add_timer(Name, Data).

remove_timer(Name) ->
	maestro_dist:remove_timer(Name).

%%====================================================================
%% Internal functions
%%====================================================================
