-module(maestro_core).

-export([
	add_shard/1,
	add_timer/3
]).


add_shard(Identifier) ->
	maestro_core_shard_sup:add_shard(Identifier).

add_timer(Shard, Name, Data) ->
	maestro_core_shard:add_timer(Shard, Name, Data).
