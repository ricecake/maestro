%%%-------------------------------------------------------------------
%% @doc maestro_core public API
%% @end
%%%-------------------------------------------------------------------

-module(maestro_core_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    maestro_core_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
