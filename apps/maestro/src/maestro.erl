-module(maestro).

%% API exports
-export([
	ping/0,
	add_timer/2,
	remove_timer/1,
        timer_status/1,
        list_timers/1
]).

%%====================================================================
%% API functions
%%====================================================================

ping() -> maestro_dist:ping().

add_timer(Name, Data) ->
	maestro_dist:add_timer(Name, Data).

remove_timer(Name) ->
	maestro_dist:remove_timer(Name).

list_timers(Opts) ->
        maestro_dist:list_timers(Opts).

timer_status(Name) ->
        maestro_dist:timer_status(Name).

%%====================================================================
%% Internal functions
%%====================================================================
