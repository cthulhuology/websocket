-module(websocket_server).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2012,2013 David J Goehrig"/utf8>>).
-behavior(gen_server).
-export([ start/3, stop/1, listen/1, retry/1, accept/2 ]).
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ]).

-define(SELF, list_to_atom(?MODULE_STRING ++ "_" ++ integer_to_list(Port))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public Behavior

-spec start(atom(),atom(),integer()) -> { atom(), pid() }.
start(Module,Function,Port) ->
	gen_server:start_link({ local, ?SELF}, ?MODULE, { Module, Function, Port }, []).

-spec stop(integer()) -> atom().
stop(Port) ->
	gen_server:call(?SELF, stop).

listen(Port) ->
	gen_server:cast(?SELF, listen).

accept(Port,Socket) ->
	gen_server:cast(?SELF, { accept, Socket }).

retry(Port) ->
	gen_server:cast(?SELF,  retry).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% gen_server

init({ Module, Function, Port }) ->
	listen(Port),
	{ ok, { Port, Module, Function } }.

handle_call(stop, _From, State) ->
        { stop, ok, State }; 		%% stop the server, terminates

handle_call(Msg, _From, State) ->
	io:format("Unknown message ~p~n", [ Msg ]),
	{ reply, ok, State }.		%% ignore unknown commands

handle_cast( listen, { Port, Module, Function }) ->
	{ ok, Listen} = gen_tcp:listen(Port,[binary,{packet,0},{reuseaddr,true},{active,true}]),
	accept(Port,Listen),
	{ noreply, { Port, Module, Function }};

handle_cast({ accept, Listen}, { Port, Module, Function }) ->
	case gen_tcp:accept(Listen) of
		{ok, Socket} -> 
			Pid  = spawn(fun() -> wait_headers(Socket,<<>>,Module,Function) end),	%% and then process headers
			ok = gen_tcp:controlling_process(Socket,Pid),				%% hand off control of the socket to wait headers
			spawn( fun() -> accept(Port,Listen) end ),				%% spawn a new acceptor
			{ noreply, { Port, Module, Function} };
		{error, closed } -> 
			retry(Port),
			{ noreply, { Port, Module, Function }}
	end;

handle_cast({ retry, Port, _Module, _Function }, State) ->	
	spawn(fun() ->
		receive
		after 1000 -> 
			listen(Port)				%% wait a second and retry 
		end
	end),
	{ noreply, State };

handle_cast(Msg, State) ->
	io:format("Unknown message ~p~n", [ Msg ]),
        { noreply, State }.		%% unused

handle_info(Info,State) ->
	io:format("Unknown message ~p~n", [ Info ]),
        { noreply, State }.		%% unused

terminate(_Reason,{ _Port, _Module, _Function }) ->
        ok.				%% no cleanup

code_change(_Old, State, _Extra) ->
        { ok, State }.                

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private Methods

%% Process HTTP headers for deciding which Protocol Version to use
headers(_Data, [], _Offset, Acc) ->
	Acc;
headers(_Data, [{ Offset, _L}| _Matches], Offset, Acc) ->
	Acc;
headers(Data, [{X,L}|Matches], Offset, Acc) ->
	Bin = binary:part(Data,Offset,X-Offset), 
	[ Key, Value ] = binary:split(Bin,<<": ">>),
	headers(Data, Matches, X+L, [{ Key, Value }|Acc]).

headers(Data) ->
	[ {X,L} | M ] = binary:matches(Data,<<"\r\n">>),
	[ Method,Path,Protocol] = string:tokens(binary:bin_to_list(binary:part(Data,0,X))," "),
	headers(Data,M,X+L,[ {method, Method }, { path, Path }, { protocol, Protocol } ]).

%% Find  the body of a HTTP request
body(Data) ->
	{O,L} = binary:match(Data,<<"\r\n\r\n">>),
	binary:part(Data,O+L,8).

%% upgrade a HTTP connection to a WebSocket
upgrade(Socket,Data,Module,Function) ->
	Headers = headers(Data),
	UUID = websocket:id(),
	{ ok, WebSocket } = case proplists:get_value(<<"Sec-WebSocket-Version">>,Headers) of
		<<"13">> -> 
			websocket:start(UUID,websocket_rfc6455, Module, Function, Socket, Headers,[]);
		_ ->
			Body = body(Data),
			websocket:start(UUID,websocket_draft00, Module, Function, Socket, Headers,Body)
	end,
	ok = gen_tcp:controlling_process(Socket,WebSocket).

contains_blank_line(Data) when is_binary(Data) ->
	case binary:matches(Data,<<"\r\n\r\n">>) of
		[] -> no;
		_ -> yes
	end.
		
wait_headers(Socket,Seen,Module,Function) ->
	receive
		{tcp, Socket, Data} ->
			case contains_blank_line(Data) of
				yes -> upgrade(Socket,<<Seen/binary,Data/binary>>,Module,Function);	
				_ -> wait_headers(Socket,<<Seen/binary,Data/binary>>,Module,Function)
			end;	
		{tcp_closed, Socket } ->
			io:format("Closed socket ~p with data ~p ~n", [ Socket, Seen ]);
		Any -> 
			io:format("TODO: fix this bug in ~p, got ~p~n", [ ?MODULE, Any ])
	end.
