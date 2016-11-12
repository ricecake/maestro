-module(maestro_dist).
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([
	ping/0,
	add_timer/2
]).

-ignore_xref([
	ping/0
]).

%% Public API

%% @doc Pings a random vnode to make sure communication is functional
ping() ->
	DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(os:timestamp())}),
	PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, maestro_dist),
	[{IndexNode, _Type}] = PrefList,
	riak_core_vnode_master:sync_spawn_command(IndexNode, ping, maestro_dist_vnode_master).

add_timer(Name, Data) when is_binary(Name), is_map(Data) ->
	{N, W} = getReplication(),
	TimeOut = timeout(),

	{ok, ReqId} = maestro_dist_op_fsm:op(N, W, {add_timer, Name, Data}, {<<"timer">>, Name}),
	receive
		{ReqId, Val} -> {ok, Val}
	after TimeOut -> {error, timeout}
	end.

getReplication() -> {3, 2}.
timeout() -> 5000.
