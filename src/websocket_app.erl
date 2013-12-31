-module(websocket_app).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2012,2013 David J. Goehrig"/utf8>>).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    websocket_sup:start_link().

stop(_State) ->
    ok.
