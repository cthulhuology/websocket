-module(websocket_logger).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2012,2013 David J. Goehrig"/utf8>>).
-export([ log/2 ]).

log(Pid,Message) ->
	UUID = websocket:uuid(Pid),
	Path = websocket:path(Pid),
	io:format("proc ~p got message [~p] from ~p id: ~p~n", [ Pid, Message, Path, UUID ]).
