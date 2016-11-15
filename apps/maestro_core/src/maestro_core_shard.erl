-module(maestro_core_shard).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
	start_link/2,
	add_timer/3,
	schedule/3
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
	[{Shard, Pid}] = ets:lookup(maestro_core_shard_registry, Shard),
	gen_server:call(Pid, {add_timer, Name, Data}).

schedule(Shard, Name, Data) ->
	[{Shard, Pid}] = ets:lookup(maestro_core_shard_registry, Shard),
	gen_server:cast(Pid, {schedule, {Name, Data}}).

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

	CallBack = fun(#{ data := Data, name := Name }) ->
		ok = maestro_core_shard:schedule(ShardIdentifier, Name, Data),
		OwnerCallback(ShardIdentifier, Name, Data)
	end,
	{ok, Timer} = watchbin:new(1000, CallBack, ShardIdentifier),

	ets:insert(maestro_core_shard_registry, {ShardIdentifier, self()}),

	{ok, #{ shard => ShardIdentifier, timer => Timer, db => DBRef, filename => FileName }}.


handle_call({add_timer, Name, Data}, _From, #{ shard := Shard } = State) ->
	lager:info("Adding timer [~s]", [Name]),
	ok = schedule_job(Shard, Name, Data),
	{reply, ok, State};
handle_call(_Msg, _From, State) ->
	{reply, ok, State}.

handle_cast({schedule, {Name, Data}}, #{ shard := Shard } = State) ->
	ok = schedule_job(Shard, Name, Data),
	{noreply, State};
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
%Spec = cronparser:time_specs("*/15 * * * *") .
%Now = calendar:local_time().
%Next = cronparser:next(Now,Spec).
%calendar:datetime_to_gregorian_seconds(Next) - calendar:datetime_to_gregorian_seconds(calendar:local_time()).

schedule_job(Timer, Name, Data) ->
	Interval = determine_interval(Data),
	lager:info("Scheduling timer [~s] for ~B milliseconds", [Name, Interval]),
	{ok, _} = watchbin:start_timer(Timer, Interval, #{ data => Data, name => Name }, [once, {name, Name}]),
	ok.

determine_interval(#{ cron := CronSpec }) ->
	Now = calendar:universal_time(),
	Spec = cronparser:time_specs(CronSpec),
	Next = cronparser:next(Now,Spec),
	Interval = calendar:datetime_to_gregorian_seconds(Next) - calendar:datetime_to_gregorian_seconds(Now),
	timer:seconds(Interval).
