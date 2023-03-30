websockets
==========

This is a websocket library for Erlang suitable for adding WebSocket support to existing applications.


Getting Started:
----------------

You can then create a series of websocket servers by adding entries in your sys.config file:

	{ websocket, [ 
		{ servers, [ { my_module, my_function, 5678 }] }
	},

This will create a websocket_server on port 5678 which will run my_module:my_function(Pid,Data) on each incoming message, where:

	* Pid - The websocket gen_server to which you can send data back
	* Data - A binary containing the body of the message

The websocket_server handles listening on the port, creating new websocket gen_server object for monitoring each incoming connection.

The websocket OTP application creates a websocket_sup supervisor tree which will restart each of your websocket_server instances on a one to one
basis.


Supported Protocols:
--------------------

Currently the websocket module support:

	* RFC 6455 websockets version 13 (http:tools.ietf.org/html/rfc6455)
	* draft hixie protocol version 00 (http:tools.ietf.org/html/draft-ietf-hybi-thewebsocketprotocol-00)

It does not support any of the optional sections, extensions, or subprotocols.  


Bugs, Comments, & Submissions:
------------------------------

If you hate this module, or love it, or would like to contribute drop me a line at:

	dave at dloh.org

LICENSE
-------

 The MIT License (MIT)
 
 Copyright (c) 2012,2013 David J Goehrig <dave@dloh.org>
 
 Permission is hereby granted, free of charge, to any person obtaining a copy of
 this software and associated documentation files (the "Software"), to deal in
 the Software without restriction, including without limitation the rights to
 use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 the Software, and to permit persons to whom the Software is furnished to do so,
 subject to the following conditions:
 
 The above copyright notice and this permission notice shall be included in all
 copies or substantial portions of the Software.
 
 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


