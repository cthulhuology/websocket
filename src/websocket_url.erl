-module(websocket_url).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-export([ parse/1, host/1, port/1, proto/1, path/1 ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%

parse(Url) ->
	{ ok, M } = re:compile("(?<Proto>[^:]+)://(?<Host>[^:]+):(?<Port>[^/]+)/(?<Path>.*)"),
	{ namelist, NL } = re:inspect(M,namelist),
	{ match, MT } = re:run(Url,M,[{capture,all_names,binary}]),
	lists:zip(NL,MT).

host(Url) ->
	proplists:get_value(<<"Host">>,parse(Url)).

port(Url) ->
	list_to_integer(binary:bin_to_list(proplists:get_value(<<"Port">>,parse(Url)))).

path(Url) ->
	proplists:get_value(<<"Path">>,parse(Url)).

proto(Url) ->
	proplists:get_value(<<"Proto">>,parse(Url)).
	

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_test() ->
	?assertMatch( [
		{<<"Host">>,<<"localhost">>},
		{<<"Path">>,<<"chat">>},
		{<<"Port">>,<<"8080">>},
		{<<"Proto">>,<<"ws">>}
	], parse("ws://localhost:8080/chat")).

host_test() ->
	?assertMatch(<<"localhost">>, host("ws://localhost:8888/chat")),
	?assertMatch(<<"127.0.0.1">>, host("ws://127.0.0.1:8888/chat")).

port_test() ->
	?assertMatch(80,port("ws://localhost:80/foobar")),
	?assertMatch(8080,port("ws://localhost:8080/chat")).
	
path_test() ->
	?assertMatch(<<"foobar">>,path("ws://localhost:80/foobar")),
	?assertMatch(<<"chat">>,path("ws://localhost:8080/chat")).

proto_test() ->
	?assertMatch(<<"ws">>,proto("ws://localhost:80/foobar")),
	?assertMatch(<<"wss">>,proto("wss://localhost:8080/chat")).
	


-endif.
