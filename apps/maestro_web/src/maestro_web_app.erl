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
			{ok, Options} = determine_options(),
			Dispatch = cowboy_router:compile([
				{'_', [
					{"/ws/",          maestro_web_msg_handler, #{}},
					{"/",             maestro_web_page, index},
					{"/static/[...]", cowboy_static, {priv_dir, maestro_web, "static/"}}
				]}
			]),
			{ok, _} = cowboy:start_clear(maestro_web, 25, Options, #{
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


determine_options() ->
        WithIp = case application:get_env(maestro_web, http_ip) of
                undefined -> [];
                {ok, IpString} when is_list(IpString) ->
                        {ok, Ip} = inet_parse:address(IpString),
                        [{ip, Ip}];
                {ok, IpTuple} when is_tuple(IpTuple) -> [{ip, IpTuple}]
        end,
        Port = application:get_env(maestro_web, http_port, 8080),
        {ok, [{port, Port} |WithIp]}.

