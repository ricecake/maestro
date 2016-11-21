-module(maestro_core).

-export([
	add_shard/2,
	add_timer/3,
	stop_shard/1,
	remove_shard/1,
	is_empty/1
]).


add_shard(Identifier, Callback) ->
	maestro_core_shard_sup:add_shard(Identifier, Callback).

add_timer(Shard, Name, Data) ->
	maestro_core_shard:add_timer(Shard, Name, Data).

stop_shard(Shard) ->
	maestro_core_shard:stop_shard(Shard).

remove_shard(Shard) ->
	maestro_core_shard:remove_shard(Shard).

is_empty(Shard) ->
	maestro_core_shard:is_empty(Shard).

