-module(maestro_dist_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case maestro_dist_sup:start_link() of
        {ok, Pid} ->
            ok = riak_core:register([{vnode_module, maestro_dist_vnode}]),
            ok = riak_core_node_watcher:service_up(maestro_dist, self()),

            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.
