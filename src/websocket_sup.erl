-module(websocket_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE,[]).

init([]) ->
	{ ok, Configs } = application:get_env(websocket,servers),
	Servers = [ { list_to_atom("websocket_server_" ++ integer_to_list(Port)),
			{ websocket_server, start, [ Module, Function, Port ]},
			permanent, 5000, worker, [ websocket_server, Module ]} || { Module, Function, Port } <- Configs ],
	{ok, { {one_for_one, 5, 10}, Servers} }.

