-module(websocket_client).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-behavior(gen_server).
-export([ start_link/3, stop/1, echo/1, send/2 ]).
-export([ code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
	terminate/2 ]).

-record(websocket_client, { socket, module, function, status, headers, data }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%

start_link(Url,Module,Function) ->
	gen_server:start_link(?MODULE, [Url,Module,Function], []).

stop(Pid) ->
	gen_server:call(Pid,stop).

send(Pid,Message) ->
	gen_server:call(Pid, { send, Message }).

echo(Message) ->
	io:format("~p~n", [ Message ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private API
%

init([Url,Module,Function]) ->
	U = websocket_url:parse(Url),
	Host = websocket_url:host(U),
	Port = websocket_url:port(U),	
	{ ok, Socket } = gen_tcp:connect(Host,Port,[{ active, true }, binary ]),
	Path = websocket_url:path(U),
	gen_tcp:send(Socket, request(Host,Path,Url)),
	{ ok, #websocket_client{ socket = Socket, module = Module, function = Function, status = upgrading, data = [] }}.

handle_call(stop,_From,Client) ->
	{ stop, stopped, Client };
handle_call({send,Message}, _From, Client = #websocket_client{ socket = Socket }) ->
	Data = websocket_rfc6455:frame(Message,1,true),
	io:format("sending ~p~n", [ Data ]),
	gen_tcp:send(Socket,Data),
	{ reply, ok, Client };
handle_call(Message,_From,Client) ->
	io:format("[websocket_client] unknown message ~p~n", [ Message ]),
	{ reply, ok, Client }.

handle_cast(Message,Client) ->
	io:format("[websocket_client] unknown message ~p~n", [ Message ]),
	{ noreply, Client }.

handle_info({ tcp, _Sock, <<"HTTP/1.1",Resp/binary>>}, Client = #websocket_client{ status = upgrading, module = Module, function = Function }) ->
	Headers = parse(Resp),
	io:format("headers ~p~n", [ Headers ]),
	case validate(<<"foobar">>,Headers) of
		true ->	spawn(Module,Function,[connected]);
		false -> spawn(Module,Function, [error])
	end,
	{ noreply, Client#websocket_client{ headers = Headers, status = connected} };

handle_info({ message, Message },  Client = #websocket_client{ status = connected, module = Module, function = Function }) ->
	spawn(Module,Function, [ Message ]),
	{ noreply, Client };

handle_info(Message, Client = #websocket_client{ socket = Socket, status = connected, data = OldData }) ->
	Data = websocket_rfc6455:handle(self(),Socket,Message,OldData),
	{ noreply, Client#websocket_client{ data = Data }};

handle_info(Message,Client) ->
	io:format("[websocket_client] unknown message ~p~n", [ Message ]),
	{ noreply, Client }.

code_change(_Old,_Extra,Client) ->
	{ ok, Client }.

terminate(_Reason,_Client) ->
	ok.

validate(Key,Headers) ->
	Resp = proplists:get_value(<<"Sec-WebSocket-Accept">>,Headers),
	Shake = <<Key/binary,"258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>, %% 25.. is magic
	Crypt = crypto:hash(sha,Shake),
	Secret = base64:encode(Crypt),
	io:format("~p vs ~p~n", [ Resp, Secret ]),
	Secret =:= Resp.
	
request(Host,Path,Origin) ->
	Key = <<"foobar">>,
	H = binary:list_to_bin(Host),
	O = binary:list_to_bin(Origin),
	<<"GET /", Path/binary, " HTTP/1.1\r\n",
	"Host: ", H/binary, "\r\n",
	"Origin: ", O/binary, "\r\n",
	"Upgrade: websocket\r\n",
	"Connection: upgrade\r\n",
	"Sec-WebSocket-Key: ", Key/binary, "\r\n",
	"Sec-WebSocket-Version: 13\r\n\r\n">>.
		
parse(Response) ->
	[ Status | Lines ] = binary:split(Response,<<"\r\n">>, [ global ]),
	io:format("got status ~p~n", [ Status ]),	
	lists:map(fun(L) ->
		case binary:split(L,<<": ">>) of
			[ K, V ] -> { K, V };
			_ -> {}
		end
	end, Lines).
