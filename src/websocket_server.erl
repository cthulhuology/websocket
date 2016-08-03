-module(websocket_server).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2012,2013,2015,2016 David J Goehrig"/utf8>>).
-behavior(gen_server).
-export([ start_link/3, stop/1, listen/1, retry/1, accept/1 ]).
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ]).

-define(SELF, list_to_atom(?MODULE_STRING ++ "_" ++ integer_to_list(Port))).
-record(websocket_server, { port, module, function, socket }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public Behavior

-spec start_link(atom(),atom(),integer()) -> { atom(), pid() }.
start_link(Module,Function,Port) ->
	gen_server:start_link({ local, ?SELF }, ?MODULE, #websocket_server{ port = Port, module = Module, function = Function }, []).

-spec stop(integer()) -> atom().
stop(Port) ->
	gen_server:call(?SELF, stop).

listen(Port) ->
	gen_server:cast(?SELF, listen).

accept(Port) ->
	gen_server:cast(?SELF, accept).

retry(Port) ->
	gen_server:cast(?SELF,  retry).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% gen_server

init(Server = #websocket_server{ port = Port }) ->
	listen(Port),
	{ ok, Server }.

handle_call(stop, _From, State) ->
        { stop, ok, State }; 		%% stop the server, terminates

handle_call(Msg, _From, State) ->
	io:format("Unknown message ~p~n", [ Msg ]),
	{ reply, ok, State }.		%% ignore unknown commands


handle_cast( listen, Server = #websocket_server{ port = Port }) ->
	{ ok, CACert } = application:get_env(cacertfile),
	{ ok, Cert } = application:get_env(certfile),
	{ ok, Key } = application:get_env(keyfile),
	case ssl:listen(Port,[
		binary, 
		{packet,0},
		{certfile, code:priv_dir(websocket) ++ "/" ++ Cert}, 
		{keyfile, code:priv_dir(websocket) ++ "/" ++ Key},
		{cacertfile, code:priv_dir(websocket) ++ "/" ++ CACert},
		{reuseaddr, true},
		{verify, verify_none}, 
		{fail_if_no_peer_cert, false},
		{versions,['tlsv1.2']},
		{ciphers,[{rsa,aes_128_cbc,sha}]}
	]) of
		{ ok, Socket } ->
			accept(Port),
			{ noreply, Server#websocket_server{ socket = Socket } };
		{ error, Reason } ->
			io:format("Failed to listen on port ~p, because ~p~n", [ Port, Reason ]),
			{ stop, Reason }
	end;

handle_cast( accept, Server = #websocket_server{ 
	port = Port, module = Module, function = Function, socket = Socket}) ->
	%% fork off a process to handle the socket
	websocket:start(Socket,Module,Function),
	%% listen for the next connection
	accept(Port),	
	{ noreply, Server };

handle_cast({ retry, Port }, State) ->	
	spawn(fun() ->
		receive
		after 1000 -> 
			listen(Port)	%% wait a second and retry 
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


