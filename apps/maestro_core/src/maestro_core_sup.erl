%%%-------------------------------------------------------------------
%% @doc maestro_core top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(maestro_core_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
%% Helper macro for declaring children of supervisor
-define(CHILD(I), #{ id => I, start => {I, start_link, []} }).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
	{ok, { #{}, [
		?CHILD(maestro_core_srv)
	]}}.

%%====================================================================
%% Internal functions
%%====================================================================
