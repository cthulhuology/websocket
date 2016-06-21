-module(websocket_url).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-export([ parse/1, host/1, port/1, proto/1, path/1, query/1 ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%

parse(Url) ->
	{ ok, M } = re:compile("(?<Proto>[^:]+)://(?<Host>[^:]+):(?<Port>[^/]+)/(?<Path>[^\?]*)(?<Query>.*)"),
	{ namelist, NL } = re:inspect(M,namelist),
	{ match, MT } = re:run(Url,M,[{capture,all_names,binary}]),
	lists:zip(NL,MT).

host(Url) ->
	binary:bin_to_list(proplists:get_value(<<"Host">>,Url)).

port(Url) ->
	list_to_integer(binary:bin_to_list(proplists:get_value(<<"Port">>,Url))).

path(Url) ->
	proplists:get_value(<<"Path">>,Url).

proto(Url) ->
	proplists:get_value(<<"Proto">>,Url).


trunc_question(<<$?,Q/binary>>) ->
	Q;
trunc_question(_) ->
	<<>>.

split_kvs(Q) ->
	binary:split(Q,<<$&>>,[global]).

split_kv(Qs) ->
	lists:map( fun(Q) ->
		case binary:split(Q,<<$=>>,[global]) of
			[ K, V ] -> { K, V };
			[ <<>> ] -> {};
			[ K ] -> { K, K };
			_ -> {}
		end
	end, Qs).
	
query(Url) ->
	Q = proplists:get_value(<<"Query">>,Url),
	Qs = trunc_question(Q),	
	Kvs = split_kvs(Qs),
	split_kv(Kvs).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_test() ->
	?assertMatch( [
		{<<"Host">>,<<"localhost">>},
		{<<"Path">>,<<"chat">>},
		{<<"Port">>,<<"8080">>},
		{<<"Proto">>,<<"ws">>},
		{<<"Query">>,<<>>}
	], parse("ws://localhost:8080/chat")).

host_test() ->
	?assertMatch("localhost", host(parse("ws://localhost:8888/chat"))),
	?assertMatch("127.0.0.1", host(parse("ws://127.0.0.1:8888/chat"))).

port_test() ->
	?assertMatch(80,port(parse("ws://localhost:80/foobar"))),
	?assertMatch(8080,port(parse("ws://localhost:8080/chat"))).
	
path_test() ->
	?assertMatch(<<"foobar">>,path(parse("ws://localhost:80/foobar"))),
	?assertMatch(<<"chat">>,path(parse("ws://localhost:8080/chat"))).

proto_test() ->
	?assertMatch(<<"ws">>,proto(parse("ws://localhost:80/foobar"))),
	?assertMatch(<<"wss">>,proto(parse("wss://localhost:8080/chat"))).

query_test() ->
	?assertMatch([
		{<<"foo">>, <<"bar">>},
		{<<"narf">>,<<"blat">>}],
		query(parse("ws://localhost:8080/chat?foo=bar&narf=blat"))),
	?assertMatch([{}],query(parse("ws://localhost:80/chat"))),
	?assertMatch([{<<"active">>,<<"active">>}],query(parse("ws://localhost:80/chat?active"))).
	
-endif.
