-module(websocket_client).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-behavior(gen_server).
-export([ start_link/3, stop/0 ]).
-export([ code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
	terminate/2 ]).

-record(websocket_client, { socket }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%

start_link(Url,Module,Function) ->
	gen_server:start_link({ local, ?MODULE }, ?MODULE, [Url,Module,Function], []).

stop() ->
	gen_server:call(?MODULE,stop).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private API
%

init([Url,Module,Function]) ->
	U = websocket_url:parse(Url),
	Host = websocket_url:host(U),
	Port = websocket_url:port(U),	
	{ ok, Socket } = gen_tcp:connect(Host,Port,[{ active, true }, binary ]),
	{ ok, #websocket_client{ socket = Socket }}.

handle_call(stop,_From,State) ->
	{ stop, stopped, State };

handle_call(Message,_From,State) ->
	io:format("[websocket_client] unknown message ~p~n", [ Message ]),
	{ reply, ok, State }.

handle_cast(Message,State) ->
	io:format("[websocket_client] unknown message ~p~n", [ Message ]),
	{ noreply, State }.

handle_info(Message,State) ->
	io:format("[websocket_client] unknown message ~p~n", [ Message ]),
	{ noreply, State }.

code_change(_Old,_Extra,State) ->
	{ ok, State }.

terminate(_Reason,_State) ->
	ok.

