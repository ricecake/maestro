-module(maestro_core).

-export([
	add_shard/2,
	add_timer/3
]).


add_shard(Identifier, Callback) ->
	maestro_core_shard_sup:add_shard(Identifier, Callback).

add_timer(Shard, Name, Data) ->
	maestro_core_shard:add_timer(Shard, Name, Data).
