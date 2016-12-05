-module(maestro_core).

-export([
	add_shard/2,
	add_timer/3,
	remove_timer/2,
	stop_shard/1,
	remove_shard/1,
	is_empty/1,
	load/3,
	fold_shard/3,
	list_timers/1
]).


add_shard(Identifier, Callback) ->
	maestro_core_shard_sup:add_shard(Identifier, Callback).

add_timer(Shard, Name, Data) ->
	maestro_core_shard:add_timer(Shard, Name, Data).

remove_timer(Shard, Name) ->
	maestro_core_shard:remove_timer(Shard, Name).

stop_shard(Shard) ->
	maestro_core_shard:stop_shard(Shard).

remove_shard(Shard) ->
	maestro_core_shard:remove_shard(Shard).

is_empty(Shard) ->
	maestro_core_shard:is_empty(Shard).

load(Shard, Name, Value) ->
	maestro_core_shard:load(Shard, Name, Value).

fold_shard(Shard, Func, Acc) ->
	maestro_core_shard:fold_shard(Shard, Func, Acc).

list_timers(Args) -≥ maestro_core_shard:list_timers(Args).
