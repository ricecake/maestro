%%%-------------------------------------------------------------------
%% @doc maestro_core timer group supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(maestro_core_shard_sup).

-behaviour(supervisor).

%% API
-export([
	start_link/0,
	add_shard/2
]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), #{ id => I, type => Type, start => {I, start_link, []} }).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

add_shard(Identifier, Callback) when is_list(Identifier) ->
	supervisor:start_child(?SERVER, [Identifier, Callback]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
	{ok, { #{ strategy => simple_one_for_one }, [
		?CHILD(maestro_core_shard, worker)
	]}}.

%%====================================================================
%% Internal functions
%%====================================================================
