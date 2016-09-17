%%%-------------------------------------------------------------------
%% @doc maestro_web public API
%% @end
%%%-------------------------------------------------------------------

-module(maestro_web_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
	case maestro_web_sup:start_link() of
		{ok, Pid} ->
			Dispatch = cowboy_router:compile([
				{'_', [
					{"/ws/",          maestro_web_msg_handler, #{}},
					{"/",             maestro_web_page, index},
					{"/static/[...]", cowboy_static, {priv_dir, maestro_web, "static/"}}
				]}
			]),
			{ok, _} = cowboy:start_clear(maestro_web, 25, [{ip, {127,0,0,1}}, {port, 8001}], #{
				env => #{ dispatch => Dispatch }
			}),
			{ok, Pid}
	end.

%%--------------------------------------------------------------------
stop(_State) ->
	ok.

%%====================================================================
%% Internal functions
%%====================================================================
