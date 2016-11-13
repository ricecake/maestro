-module(maestro_core_shard).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
	start_link/2,
	add_timer/3
]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(ShardIdentifier, Callback) ->
	gen_server:start_link(?MODULE, {ShardIdentifier, Callback}, []).

add_timer(Shard, Name, Data) ->
	gen_server:call(Shard, {add_timer, Name, Data}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init({ShardIdentifier, OwnerCallback}) ->
	FileName = filename:join(["maestro_core_shard_data", ShardIdentifier]),
	ok = filelib:ensure_dir(FileName),

	{ok, DBRef} = eleveldb:open(FileName, [
		{create_if_missing, true},
		{compression, true},
		{use_bloomfilter, true}
	]),

	CallBack = fun(Data) ->
		OwnerCallback(ShardIdentifier, Data)
	end,
	{ok, Timer} = watchbin:new(1000, CallBack),


	{ok, #{ timer => Timer, db => DBRef, filename => FileName }}.


handle_call({add_timer, Name, Data}, _From, #{ timer := Timer } = State) ->
	Interval = 5000,
	{ok, _} = watchbin:start_timer(Timer, Interval, #{ data => Data, name => Name }, [{name, Name}]),
	{reply, ok, State};
handle_call(_Msg, _From, State) ->
	{reply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
%maestro_dist:add_timer(<<"test">>, #{}).
