-module(maestro_core_shard).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(ShardIdentifier) ->
	gen_server:start_link(?MODULE, ShardIdentifier, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(ShardIdentifier) ->
	FileName = filename:join(["maestro_core_shard_data", ShardIdentifier]),
	ok = filelib:ensure_dir(FileName),

	{ok, DBRef} = eleveldb:open(FileName, [
		{create_if_missing, true},
		{compression, true},
		{use_bloomfilter, true}
	]),

	CallBack = fun(_Data) ->
		lager:debug("Cron tick -- " ++ ShardIdentifier)
	end,
	{ok, Timer} = watchbin:new(2500, CallBack),


	{ok, #{ timer => Timer, db => DBRef, filename => FileName }}.


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
