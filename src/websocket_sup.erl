-module(websocket_sup).
-behaviour(supervisor).
-export([start_link/0, server/3 ]).
-export([init/1]).

-define(WEBSOCKET_SERVER(P), list_to_atom("websocket_server_" ++ integer_to_list(P))).


start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE,[]).

init([]) ->
	{ ok, { {one_for_one, 5, 10}, [] } }.


server(Module,Function,Port) ->
	supervisor:start_child(?MODULE, #{ 
		id =>  ?WEBSOCKET_SERVER(Port),
		start => { websocket_server, start_link, [ Module, Function, Port ]},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [ 
			websocket_server, 
			websocket, 
			websocket_rfc6455 
		]}).
