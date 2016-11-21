-module(maestro_core_shard).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
	start_link/2,
	add_timer/3,
	schedule/3,
	stop_shard/1,
	remove_shard/1,
	is_empty/1
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

stop_shard(Shard) ->
	[{Shard, Pid}] = ets:lookup(maestro_core_shard_registry, Shard),
	gen_server:call(Pid, stop).

remove_shard(Shard) ->
	[{Shard, Pid}] = ets:lookup(maestro_core_shard_registry, Shard),
	gen_server:call(Pid, remove).

is_empty(Shard) ->
	[{Shard, Pid}] = ets:lookup(maestro_core_shard_registry, Shard),
	gen_server:call(Pid, is_empty).

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
	{ok, _Timer} = watchbin:new(1000, CallBack, ShardIdentifier),

	State = #{
		shard    => ShardIdentifier,
		timer    => ShardIdentifier,
		db       => DBRef,
		filename => FileName
	},

	eleveldb:fold(DBRef, fun({Key, Value}, _) ->
		Name = binary_to_term(Key),
		Data = binary_to_term(Value),
		ok = schedule_job(Name, Data, State)
	end, ok, []),

	ets:insert(maestro_core_shard_registry, {ShardIdentifier, self()}),

	{ok, State}.


handle_call({add_timer, Name, Data}, _From, #{ db := Db } = State) ->
	lager:info("Adding timer [~s]", [Name]),
	ok = store(Db, Name, Data),
	ok = schedule_job(Name, Data, State),
	{reply, ok, State};
handle_call(stop, _From, State) ->
	{stop, normal, ok, State};
handle_call(remove, _From, #{ db := Db, filename := File, timer := Timer } = State) ->
	watchbin:destroy(Timer),
	eleveldb:close(Db),
	{Result, NewState} = case eleveldb:destroy(File, []) of
		ok ->
			{ok, State#{ db := undefined, timer := undefined }};
		{error, Reason} ->
			{{error, Reason}, State}
	end,
	{reply, Result, NewState};
handle_call(is_empty, _From, #{ db := Db } = State) ->
	{reply, eleveldb:is_empty(Db), State};
handle_call(_Msg, _From, State) ->
	{reply, ok, State}.

handle_cast({schedule, {Name, Data}}, State) ->
	ok = schedule_job(Name, Data, State),
	{noreply, State};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, #{ timer := Timer, db := Db }) ->
	case Timer of
		undefined -> ok;
		_         ->
			watchbin:destroy(Timer)
	end,
	case Db of
		undefined -> ok;
		_         ->
			eleveldb:close(Db)
	end,
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

schedule_job(Name, Data, #{ shard := Timer, db := Db }) ->
	case determine_interval(Data) of
		Interval when is_integer(Interval) andalso Interval > 0 ->
			lager:info("Scheduling timer [~s] for ~B milliseconds", [Name, Interval]),
			{ok, _} = watchbin:start_timer(Timer, Interval, #{ data => Data, name => Name }, [once, {name, Name}]);
		_Undefined ->
			lager:info("Timer [~s] not resecheduled: undefined next interval", [Name]),
			delete(Db, Name)
	end,
	ok.

determine_interval(#{ at := DateTime }) ->
	Now = calendar:universal_time(),
	Interval = calendar:datetime_to_gregorian_seconds(DateTime) - calendar:datetime_to_gregorian_seconds(Now),
	timer:seconds(Interval);
determine_interval(#{ interval := {Unit, Num}}) ->
	timer:Unit(Num);
determine_interval(#{ cron := CronSpec }) ->
	Now = calendar:universal_time(),
	Spec = cronparser:time_specs(CronSpec),
	Next = cronparser:next(Now,Spec),
	Interval = calendar:datetime_to_gregorian_seconds(Next) - calendar:datetime_to_gregorian_seconds(Now),
	timer:seconds(Interval).

store(Db, Key, Value) -> eleveldb:put(Db, term_to_binary(Key), term_to_binary(Value), []).

%exists(Db, Key) ->
%        case eleveldb:get(Db, term_to_binary(Key), []) of
%                {ok, _BinaryTerm}  -> true;
%                not_found         -> false
%        end.
%
%getIfExists(Db, Key) ->
%        case eleveldb:get(Db, term_to_binary(Key), []) of
%                {ok, BinaryTerm} -> {ok, binary_to_term(BinaryTerm)};
%                not_found         -> false
%        end.

delete(Db, Key) ->
        eleveldb:delete(Db, term_to_binary(Key), []).

