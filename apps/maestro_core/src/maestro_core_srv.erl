-module(maestro_core_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, #{}, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(State) ->
	File = filename:join(code:priv_dir(maestro_web), "static/midi/bumble_bee.mid"),
	{seq, {header, _Version, Devision}, {track, Tracks}, OtherTracks} = midifile:read(File),
	TicksPerQuarterNote = case <<Devision:16>> of
		<<0:1, TPQN:15>> -> TPQN;
		<<1:1, FPS:7, TPF:8>> ->  FPS*TPF
	end,
	AllTracks = lists:flatten([Tracks, [ TrackData || {track, TrackData} <- OtherTracks ]]),
	{ok, State#{ track => AllTracks, tpqs => TicksPerQuarterNote, interval => 500000, ts_denom => 4, ts_num => 4, tpqs => 256 }}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({midi, Type, Data}, #{ track := [Next |Rest] } = State) ->
	midiEvent(State, Next),
	{ok, NewState} = handleMIDI(Type, Data, State#{ track := Rest }),
	{noreply, NewState};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

handleMIDI(time_signature, [<<Num, Denom, _, _>>], State) ->
	{ok, State#{ ts_denom := math:pow(2, Denom), ts_num => Num }};
handleMIDI(tempo, [TICKS], State) ->
	{ok, State#{ interval := TICKS }};
handleMIDI(on, [_Channel, _Note, _Velocity], State) ->
	%{reply, {text, formatMessage(<<"note.on">>, #{ note => Note, channel => Channel, velocity => Velocity })}, State},
	{ok, State};
handleMIDI(off, [_Channel, _Note, _Velocity], State) ->
	%{reply, {text, formatMessage(<<"note.off">>, #{ note => Note, channel => Channel, velocity => Velocity })}, State},
	{ok, State};
handleMIDI(program, [_Channel, _Program], State) ->
	%{reply, {text, formatMessage(<<"control.program">>, #{ channel => Channel, program => Program })}, State},
	{ok, State};
handleMIDI(_, _, State) ->
	{ok, State}.

midiEvent(#{ interval := Interval, tpqs := TPQN }, {Type, Delay, Data}) ->
	SecondsPerQuarterNote = Interval / 1000,
	SecondsPerTick = SecondsPerQuarterNote / TPQN,
	EventDelay = round(Delay * SecondsPerTick),

	erlang:send_after(EventDelay, self(), {midi, Type, Data}),
	ok.
