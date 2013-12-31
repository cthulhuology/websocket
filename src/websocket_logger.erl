-module(websocket_logger).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2012,2013 David J. Goehrig"/utf8>>).
-export([ log/2 ]).

log(Pid,Message) ->
	io:format("~p got message [~p]~n", [ Pid, Message ]).
