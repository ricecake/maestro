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

init(State) -> initMidiState(State).


handle_call(register, {From, _UID}, #{ clients := Clients } = State) ->
	erlang:monitor(process, From),
	{ok, NewState} = case Clients of
		[] ->
			#{ track := [ First | Rest] } = State,
			{midiEvent(State, First), State#{ track := Rest }};
		_  -> {ok, State}
	end,
	{reply, ok, NewState#{ clients := [ From |Clients] }};
handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({midi, Type, Data}, #{ track := [Next |Rest] } = State) ->
	midiEvent(State, Next),
	{ok, NewState} = handleMIDI(Type, Data, State#{ track := Rest }),
	{noreply, NewState};
handle_info({'DOWN', _, _, Pid, _}, #{ clients := Clients } = State) ->
	{noreply, State#{ clients := Clients -- [Pid]}};
handle_info(Info, State) ->
	io:format("UNHANDLED: ~p~n", [Info]),
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
handleMIDI(on, [Channel, Note, Velocity], #{ clients := Clients } = State) ->
	Handlers = lrw:top({Channel, Note}, Clients, 1),
	[maestro_web_msg_handler:send(Handler, <<"note.on">>, #{ note => Note, channel => Channel, velocity => Velocity }) || Handler <- Handlers],
	{ok, State};
handleMIDI(off, [Channel, Note, Velocity], #{ clients := Clients } = State) ->
	Handlers = lrw:top({Channel, Note}, Clients, 1),
	[maestro_web_msg_handler:send(Handler, <<"note.off">>, #{ note => Note, channel => Channel, velocity => Velocity }) || Handler <- Handlers],
	{ok, State};
handleMIDI(program, [Channel, Program], #{ clients := Clients } = State) ->
	[maestro_web_msg_handler:send(Handler, <<"control.program">>, #{ channel => Channel, program => Program }) || Handler <- Clients],
	{ok, State};
handleMIDI(_, _, State) ->
	{ok, State}.

midiEvent(#{ interval := Interval, tpqs := TPQN }, {Type, Delay, Data}) ->
	SecondsPerQuarterNote = Interval / 1000,
	SecondsPerTick = SecondsPerQuarterNote / TPQN,
	EventDelay = round(Delay * SecondsPerTick),

	erlang:send_after(EventDelay, self(), {midi, Type, Data}),
	ok.


initMidiState(State) ->
	File = filename:join(code:priv_dir(maestro_web), "static/midi/bumble_bee.mid"),
	{seq, {header, _Version, Devision}, {track, Tracks}, OtherTracks} = midifile:read(File),
	TicksPerQuarterNote = case <<Devision:16>> of
		<<0:1, TPQN:15>> -> TPQN;
		<<1:1, FPS:7, TPF:8>> ->  FPS*TPF
	end,
	AllTracks = lists:flatten([Tracks, [ TrackData || {track, TrackData} <- OtherTracks ]]),
	{ok, State#{
		track    => AllTracks,
		tpqs     => TicksPerQuarterNote,
		interval => 500000,
		ts_denom => 4,
		ts_num   => 4,
		tpqs     => 256,
		clients  => []
	}}.
