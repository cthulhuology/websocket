-module(websocket).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2012,2013 David J. Goehrig"/utf8>>).
-behavior(gen_server).
-export([ start/3, send/2, socket/1, headers/1, path/1, stop/1, uuid/1, method/1, protocol/1,
	wait_headers/2, upgrade/2, bind/3  ]).
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ]).

-record(websocket, { uuid, protocol, headers, socket, data, module, function, connecting }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public Methods
%%

%% Starts a websocket by accepting a connection form Listen port
start(Listen,Module,Function) ->
	gen_server:start_link(?MODULE, { Listen, Module, Function }, []).

%% Waits for the HTTP headers to determine if we can upgrade
wait_headers(WebSocket,Data) ->
	gen_server:cast(WebSocket,{ wait_headers, Data }).

%% Upgrades to the WebSocket of the appropriate protocol
upgrade(WebSocket,Data) ->
	gen_server:cast(WebSocket,{ upgrade, Data }).

%% Sends data to the websocket
send(WebSocket,Data) ->
	gen_server:call(WebSocket,{ send, Data }).

%% Returns the UUID of the WebSocket
uuid(WebSocket) ->
	gen_server:call(WebSocket,uuid).

%% Returns the raw TCP socket of the WebSocket
socket(WebSocket) ->
	gen_server:call(WebSocket,socket).

%% Returns the HTTP headers of the WebSocket
headers(WebSocket) ->
	gen_server:call(WebSocket,headers).

%% Returns the path of the WebSocket
path(WebSocket) ->
	gen_server:call(WebSocket,path).

%% Returns the method of the WebSocket
method(WebSocket) ->
	gen_server:call(WebSocket,method).

%% Returns the protocol of the WebSocket
protocol(WebSocket) ->
	gen_server:call(WebSocket,protocol).

%% Stops the WebSocket
stop(WebSocket) ->
	gen_server:cast(WebSocket,stop).

%% Binds a Module and Function to be called when a message is encountered
bind(WebSocket,Module,Function) ->
	gen_server:cast(WebSocket, { bind, Module, Function }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server methods
init({ Listen, Module, Function }) ->
	%% accept the socket in this process
	case gen_tcp:accept(Listen) of
		{ ok, Socket } -> 
			%% if we get a socket, wait for the headers
			wait_headers(self(), <<"">>),
			{ ok, #websocket{ 
				uuid = uuid:id(), 
				socket = Socket, 
				headers = [], 
				data = [],
				module = Module,
				function = Function,
				connecting = true
			}};
		{ error, closed } ->
			%% if we don't get a socket, just bail!
			{ stop, closed }
	end.

handle_call({ send, Data } , _From, WebSocket = #websocket{ protocol = Protocol, socket = Socket }) ->
	ok = gen_tcp:send(Socket,Protocol:frame(Data)),
	{ reply, ok, WebSocket };

handle_call( uuid, _From, WebSocket = #websocket{ uuid = UUID }) ->
	{ reply, UUID, WebSocket };

handle_call( socket, _From, WebSocket = #websocket{ socket = Socket }) ->
	{ reply, Socket, WebSocket };

handle_call( headers, _From, WebSocket = #websocket{ headers = Headers }) ->
	{ reply, Headers, WebSocket };

handle_call( path, _From, WebSocket = #websocket{ headers = Headers }) ->
	{ path, Path } = lists:keyfind(path,1,Headers),
	{ reply, Path, WebSocket };

handle_call( method, _From, WebSocket = #websocket{ headers = Headers }) ->
	{ method, Method } = lists:keyfind(method,1,Headers),
	{ reply, Method, WebSocket };

handle_call( protocol, _From, WebSocket = #websocket{ protocol = Protocol }) ->
	{ reply, Protocol, WebSocket }.

handle_cast({ bind, Module, Function }, WebSocket) ->
	{ noreply, WebSocket#websocket{ module = Module, function = Function }};

handle_cast( stop, WebSocket ) ->
	{ stop, normal, WebSocket };

handle_cast({ wait_headers, Seen }, WebSocket) ->
	{ noreply, WebSocket#websocket{ data = Seen, connecting = true }};

handle_cast({ upgrade, Data }, WebSocket = #websocket{ socket = Socket, module = Module, function = Function }) ->
	%% parse the headers
	Headers = parse_headers(Data),
	%% determine which protocol to use, draft00 is now deprecated
	Protocol =  case proplists:get_value(<<"Sec-WebSocket-Version">>,Headers) of
		<<"13">> -> 
			io:format("using rfc6455~n"),
			websocket_rfc6455;
		_ ->
			io:format("Protocol not supported")
	end,
	%% generate the server handshake
	Handshake = Protocol:handshake(Headers,Data),
	%% find if we have any websocket data already in hand
	%% send the handshake
	case gen_tcp:send(Socket,Handshake) of
		ok -> 
			spawn(Module,Function,[self(),connected]),
			{ noreply, WebSocket#websocket{ 
				protocol = Protocol,
				headers = Headers, 
				data = [],
				connecting = false
			}};
		{ error, Reason } ->	
			io:format("Handshake failed ~p~n", [ Reason ]),
			{ stop, handshake_failed, WebSocket }
	end.

handle_info({tcp, _Socket, Data}, WebSocket = #websocket{ connecting = true, data = Seen }) ->
	case contains_blank_line(<<Seen/binary,Data/binary>>) of
		yes -> 
			upgrade(self(),<<Seen/binary,Data/binary>>),
			{ noreply, WebSocket };
		_ -> 
			wait_headers(self(),<<Seen/binary,Data/binary>>),
			{ noreply, WebSocket }
	end;	

handle_info({tcp_closed, Socket }, WebSocket = #websocket{ data = Seen, connecting = true }) ->
	io:format("Closed socket ~p with data ~p ~n", [ Socket, Seen ]),
	{ stop, normal, WebSocket };

handle_info({ message, Data }, WebSocket = #websocket{ module = Module, function = Function, uuid = UUID }) ->	
	io:format("[websocket] Got message ~p~n", [ Data ]),
	io:format("invoking ~p:~p for ~p~n", [ Module, Function, UUID ]),
	spawn(Module,Function,[ self(), Data ] ),
	{ noreply, WebSocket#websocket{ data = [] }};

handle_info({ send, Data }, WebSocket = #websocket { protocol = Protocol, socket = Socket }) ->
	ok = gen_tcp:send(Socket,Protocol:frame(Data)),
	{ noreply, WebSocket };

handle_info({ ping, _Socket }, WebSocket = #websocket{ protocol = Protocol, socket = Socket }) -> 
	gen_tcp:send(Socket,Protocol:frame(<<"pong">>,10)),	%% send pong
	{ noreply, WebSocket };

handle_info({ pong, _Socket },WebSocket) ->
	{ noreply, WebSocket };

handle_info({ unknown, Any }, WebSocket) ->
	io:format("Unknown message ~p~n", [ Any ]),
	{ stop, unknown_message, WebSocket };

handle_info({ close, _Socket }, WebSocket = #websocket{ }) ->
	%% after a second stop the websocket
	timer:apply_after(1000, ?MODULE, stop, [ self() ]),
	{ noreply, WebSocket };

handle_info( Message, WebSocket = #websocket{ protocol = Protocol, socket = Socket, data = Data }) ->
	io:format("[websocket] message  ~p~n", [ Message ]),
	NewData = Protocol:handle(self(),Socket,Message,Data),
	io:format("[websocket] new data ~p~n", [ NewData ]),
	{ noreply, WebSocket#websocket{ data = NewData }}.

terminate( normal, #websocket{ uuid = UUID, socket = Socket, module = Module, function = Function }) ->
	spawn(Module,Function,[UUID, closed]),
	gen_tcp:close(Socket),
	io:format("Closed socket ~p~n", [ UUID ]),
	ok;

terminate( Reason, #websocket{ uuid = UUID, socket = Socket, module = Module, function = Function }) ->
	io:format("Terminating socket ~p with reason ~p~n", [ UUID, Reason ]),
	spawn(Module,Function,[UUID, closed]),
	gen_tcp:close(Socket),
	ok.

code_change( _Old, WebSocket, _Extra ) ->
	{ ok, WebSocket }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private Methods

%% Process HTTP headers for deciding which Protocol Version to use
parse_headers( _Data, [], _Offset, Acc ) ->
	Acc;
parse_headers( _Data, [{ Offset, _L}| _Matches], Offset, Acc ) ->
	Acc;
parse_headers( Data, [{X,L}|Matches], Offset, Acc ) ->
	Bin = binary:part(Data,Offset,X-Offset), 
	[ Key, Value ] = binary:split(Bin,<<": ">>),
	parse_headers(Data, Matches, X+L, [{ Key, Value }|Acc]).

parse_headers( Data ) ->
	[ {X,L} | M ] = binary:matches(Data,<<"\r\n">>),
	[ Method,Path,Protocol] = string:tokens(binary:bin_to_list(binary:part(Data,0,X))," "),
	parse_headers(Data,M,X+L,[ {method, Method }, { path, Path }, { protocol, Protocol } ]).

%% determines if we reached the end of the HTTP headers
contains_blank_line( Data ) when is_binary( Data ) ->
	case binary:matches(Data,<<"\r\n\r\n">>) of
		[] -> no;
		_ -> yes
	end.
