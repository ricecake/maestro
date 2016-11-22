-module(maestro_dist_vnode).
-behaviour(riak_core_vnode).

-export([
	start_vnode/1,
	init/1,
	terminate/2,
	handle_command/3,
	is_empty/1,
	delete/1,
	handle_handoff_command/3,
	handoff_starting/2,
	handoff_cancelled/1,
	handoff_finished/2,
	handle_handoff_data/2,
	encode_handoff_item/2,
	handle_coverage/4,
	handle_exit/3
]).

-ignore_xref([
	start_vnode/1
]).

%% API
start_vnode(I) ->
	riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
	{ok, Shard} = maestro_core:add_shard(integer_to_list(Partition), fun check_owner/3),
	{ok, #{ partition => Partition, shard => Shard }}.

%% Sample command: respond to a ping
handle_command(ping, _Sender, #{ partition := Partition } = State) ->
	{reply, {pong, Partition}, State};
handle_command({RefId, {add_timer, Name, Data}}, _Sender, #{ partition := Partition, shard := Shard } = State) ->
	ok = maestro_core:add_timer(Shard, Name, Data),
	{reply, {RefId, {timing, Partition}}, State};
handle_command({RefId, {remove_timer, Name}}, _Sender, #{ partition := Partition, shard := Shard } = State) ->
	ok = maestro_core:remove_timer(Shard, Name),
	{reply, {RefId, {removed, Partition}}, State};
handle_command(Message, _Sender, State) ->
	lager:warning("unhandled_command ~p", [Message]),
	{noreply, State}.

handle_handoff_command(_Message, _Sender, State) ->
	{noreply, State}.

handoff_starting(_TargetNode, State) ->
	{true, State}.

handoff_cancelled(State) ->
	{ok, State}.

handoff_finished(_TargetNode, State) ->
	{ok, State}.

handle_handoff_data(_Data, State) ->
	{reply, ok, State}.

encode_handoff_item(Name, Value) ->
	term_to_binary({Name, Value}).

is_empty(#{ shard := Shard } = State) ->
	{maestro_core:is_empty(Shard), State}.

delete(#{ shard := Shard } = State) ->
	maestro_core:remove_shard(Shard),
	{ok, State}.

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
	{stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
	{noreply, State}.

terminate(_Reason, #{ shard := Shard }) ->
	maestro_core:stop_shard(Shard),
	ok.

check_owner(Partition, Name, Data) ->
	{ok, Primary, Secondaries} = maestro_dist:find_primaries({<<"timer">>, Name}),
	ThisVnode = {list_to_integer(Partition), node()},
	ok = case Primary of
		ThisVnode  ->
			{ok, Ring} = riak_core_ring_manager:get_my_ring(),
			_RunningNodes = riak_core_node_watcher:nodes(maestro_dist),
			_AllNodes = riak_core_ring:all_members(Ring),
			lager:info("Executing Task for job [~s]", [Name]),
			antiEntropy({Name, Data}, Secondaries);
		_OtherVnode -> antiEntropy({Name, Data}, [Primary | [Node || Node <- Secondaries, Node /= ThisVnode]])
	end.


antiEntropy({Name, Data}, Vnodes) ->
	ReqId = erlang:phash2(os:timestamp()),
	riak_core_vnode_master:command(Vnodes, {ReqId, {add_timer, Name, Data}}, dtimer_vnode_master).
