-module(websocket_logger).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2012,2013,2016 David J. Goehrig"/utf8>>).
-export([ log/3 ]).

log(Pid,Path, Message) ->
	UUID = websocket:uuid(Pid),
	io:format("proc ~p got message [~p] from ~p id: ~p~n", [ Pid, Message, Path, UUID ]).
