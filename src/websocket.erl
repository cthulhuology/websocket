-module(websocket).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2012,2013 David J. Goehrig"/utf8>>).
-behavior(gen_server).
-export([ start/7, send/2, socket/1, stop/1, id/0, uuid/1 ]).
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ]).

-define(SELF, list_to_atom(UUID  ++ "_websocket")).
-record(websocket, { uuid, protocol, module, function, socket, header, data }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public Methods

id() ->
	uuid:to_string(uuid:v4()).

uuid(WebSocket) ->
	gen_server:call(WebSocket,uuid).

start(UUID,Protocol,Module,Function,Socket,Header,Data) ->
	gen_server:start_link({local, ?SELF}, ?MODULE, #websocket{ uuid = UUID, protocol = Protocol, module = Module, function = Function, socket = Socket, header = Header, data = Data}, []).

send(UUID,Data) ->
	gen_server:call(?SELF,{send, Data}).

socket(UUID) ->
	gen_server:call(?SELF,socket).

stop(UUID) ->
	gen_server:cast(?SELF,stop).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private methods
init(WebSocket) ->
	UUID = WebSocket#websocket.uuid,
	gen_server:cast(?SELF,upgrade),
	{ ok, WebSocket }.

handle_call({send, Data } ,_From,State) ->
	Protocol = State#websocket.protocol,
	ok = gen_tcp:send(State#websocket.socket,Protocol:frame(Data)),
	{ reply, ok, State };
handle_call(uuid,_From,State) ->
	{ reply, State#websocket.uuid, State };
handle_call(socket,_From,State) ->
	{ reply, State#websocket.socket, State }.

handle_cast(stop,State) ->
	{ stop, normal, State };
handle_cast(upgrade,State) ->
	Protocol = State#websocket.protocol,
	Handshake = Protocol:handshake(State#websocket.header,State#websocket.data),
	case gen_tcp:send(State#websocket.socket,Handshake) of
		ok -> 
			{ noreply, State };
		{ error, Reason } ->	
			io:format("Handshake failed ~p~n", [ Reason ]),
			{ stop, handshake_failed, State }
	end.

handle_info({ message, Data },State) ->	
	spawn(State#websocket.module,State#websocket.function,[ self(), Data ] ),
	{ noreply, State#websocket{ data = [] }};
handle_info({ send, Data },State) ->
	Protocol = State#websocket.protocol,
	ok = gen_tcp:send(State#websocket.socket,Protocol:frame(Data)),
	{ noreply, State };
handle_info({ ping, _Socket },State) -> 
	Protocol = State#websocket.protocol,
	gen_tcp:send(State#websocket.socket,Protocol:frame(<<"pong">>,10)),	%% send pong
	{ noreply, State };
handle_info({ pong, _Socket },State) ->
	{ noreply, State };
handle_info({ unknown, Any },State) ->
	io:format("Unknown message ~p~n", [ Any ]),
	{ stop, unknown_message, State };
handle_info({ close, _Socket },State) ->
	io:format("Socket closed ~p~n", [State#websocket.uuid]),
	stop(State#websocket.uuid),	
	{ noreply, State };
handle_info(Message,State) ->
	Protocol = State#websocket.protocol,
	NewData = Protocol:handle(self(),State#websocket.socket,Message,State#websocket.data),
	{ noreply, State#websocket{ data = NewData }}.

terminate(normal,State) ->
	io:format("Closing socket ~p~n", [ State#websocket.uuid ]),
	gen_tcp:close(State#websocket.socket),
	ok;
terminate(Reason,State) ->
	io:format("Terminating socket ~p with reason ~p~n", [ State#websocket.uuid, Reason ]),
	gen_tcp:close(State#websocket.socket),
	ok.

code_change(_Old,State,_Extra) ->
	{ ok, State }.
