-module(maestro_web_msg_handler).

%% Cowboy callback
-export([
	init/2,
	websocket_init/1,
	websocket_handle/2,
	websocket_info/2,
	terminate/3
]).

%% Api Exports
-export([
	send/3
]).

%% ===================================================================
%% Cowboy callbacks
%% ===================================================================

init(Req, Opts) ->
	{ok, Req2, State} = initSession(Req, Opts),
	{cowboy_websocket, Req2, State}.

websocket_init(State) ->
	File = filename:join(code:priv_dir(maestro_web), "static/midi/bumble_bee.mid"),
	{seq, {header, _Version, Devision}, {track, Tracks}, OtherTracks} = midifile:read(File),
	TicksPerQuarterNote = case <<Devision:16>> of
		<<0:1, TPQN:15>> -> TPQN;
		<<1:1, FPS:7, TPF:8>> ->  FPS*TPF
	end,
	AllTracks = lists:flatten([Tracks, [ TrackData || {track, TrackData} <- OtherTracks ]]),
	{ok, State#{ track => AllTracks, tpqs => TicksPerQuarterNote, interval => 500000, ts_denom => 4, ts_num => 4, tpqs => 256 }}.

websocket_handle({text, JSON}, State) ->
	case jsx:decode(JSON, [return_maps]) of
		#{ <<"type">> := Type } = Message ->
			Content = maps:get(<<"content">>, Message, #{}),
			{ok, NewState} = handle_client_task(Type, Content, State),
			{ok, NewState};
		_ -> {ok, State}
	end;
websocket_handle(_Frame, State) ->
	{ok, State}.

websocket_info({midi, Type, Data}, #{ track := [Next |Rest] } = State) ->
	midiEvent(State, Next),
	handleMIDI(Type, Data, State#{ track := Rest });
websocket_info({send, Message}, State) ->
	{reply, {text, Message}, State};
websocket_info(_Message, State) ->
	{ok, State}.

terminate(_, Req, State) ->
	{ok, Req, State}.


handleMIDI(time_signature, [<<Num, Denom, _, _>>], State) ->
	{ok, State#{ ts_denom := math:pow(2, Denom), ts_num => Num }};
handleMIDI(tempo, [TICKS], State) ->
	{ok, State#{ interval := TICKS }};
handleMIDI(on, [Channel, Note, Velocity], State) ->
	{reply, {text, formatMessage(<<"note.on">>, #{ note => Note, channel => Channel, velocity => Velocity })}, State};
handleMIDI(off, [Channel, Note, Velocity], State) ->
	{reply, {text, formatMessage(<<"note.off">>, #{ note => Note, channel => Channel, velocity => Velocity })}, State};
handleMIDI(program, [Channel, Program], State) ->
	{reply, {text, formatMessage(<<"control.program">>, #{ channel => Channel, program => Program })}, State};
handleMIDI(_, _, State) ->
	{ok, State}.

formatMessage(Type, Message) -> jsx:encode(#{ type => Type, content => Message }).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @doc Send a message to a websocket handler
%%
%% This method will send a message to a websocket handler for direct
%% propagation to connected client.
%% Offloads the burden of encoding the message to the sender, so messages
%% should be encodable as JSON.
%%
%% @end

-spec send(Handler :: pid(), Type :: binary(), Message :: map()) -> ok.

send(Handler, Type, Message) ->
	Handler ! {send, jsx:encode(#{ type => Type, content => Message })},
	ok.

midiEvent(#{ interval := Interval, tpqs := TPQN }, {Type, Delay, Data}) ->
	SecondsPerQuarterNote = Interval / 1000,
	SecondsPerTick = SecondsPerQuarterNote / TPQN,
	EventDelay = round(Delay * SecondsPerTick),

	erlang:send_after(EventDelay, self(), {midi, Type, Data}),
	ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

initSession(Req, State) ->
	{ok, Req, State}.

handle_client_task(<<"client.ready">>, _Content, #{ track := [ First | Rest] } = State) ->
	midiEvent(State, First),
	{ok, State#{ track := Rest }};
handle_client_task(Type, Content, State) ->
	lager:error("Untracked Message: ~p~n", [{Type, Content}]),
	{ok, State}.
