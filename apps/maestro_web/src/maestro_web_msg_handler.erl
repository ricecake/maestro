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
	{ok, State}.

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

websocket_info({send, Message}, State) ->
	{reply, {text, Message}, State};
websocket_info(_Message, State) ->
	{ok, State}.

terminate(_, Req, State) ->
	{ok, Req, State}.

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

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

initSession(Req, State) ->
	{ok, Req, State}.

handle_client_task(<<"client.ready">>, _Content, State) ->
	ok = gen_server:call(maestro_core_srv, register),
	{ok, State};
handle_client_task(Type, Content, State) ->
	lager:error("Untracked Message: ~p~n", [{Type, Content}]),
	{ok, State}.
