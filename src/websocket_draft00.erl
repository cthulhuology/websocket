-module(websocket_draft00).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2012,2013 David J Goehrig"/utf8>>).
-export([ handshake/2, frame/1, frame/2, frame/3, handle/4, unframe/4 ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API

frame(Data) when is_binary(Data) ->
	lists:flatten([ 0, binary:bin_to_list(Data), 255]).

frame(Data,_Opcode) -> frame(Data).

frame(Data,_Opcode,_Masked) -> frame(Data).

handle(Pid,Socket,Message,OldData) ->
	case Message of
		{tcp, Socket, NewData} ->
			unframe(Pid,Socket,OldData,NewData);
		{tcp_closed, Socket } ->
			Pid ! { close, Socket },
			OldData;
		Any ->
			io:format("Unknown message ~p~n", [ Any ]),
			OldData
	end.
	
%% draft-ietf-hybi-thewebsocketprotocol-00 handshake
handshake(Headers,Body) ->
	Key1 = list_to_integer(numbers(proplists:get_value(<<"Sec-WebSocket-Key1">>,Headers))) div spaces(proplists:get_value(<<"Sec-WebSocket-Key1">>,Headers)),
	Key2 = list_to_integer(numbers(proplists:get_value(<<"Sec-WebSocket-Key2">>,Headers))) div spaces(proplists:get_value(<<"Sec-WebSocket-Key2">>,Headers)),
	Key = <<Key1:32, Key2:32, Body/binary>>,
	Origin = binary:bin_to_list(proplists:get_value(<<"Origin">>, Headers)),
	Location = binary:bin_to_list(proplists:get_value(<<"Host">>,Headers)),
	Protocol = proplists:get_value(protocol,Headers),
	Path = proplists:get_value(path,Headers),
	 [
		Protocol, " 101 Web Socket Protocol Handshake\r\n",
		"Upgrade: WebSocket\r\n",
		"Connection: Upgrade\r\n",
		"Sec-WebSocket-Origin: ", Origin, "\r\n",
		"Sec-WebSocket-Location: ", "  ws://", Location, Path,"\r\n"
		"Sec-WebSocket-Protocol: ", "syndi\r\n",
		"\r\n",
		crypto:hash(md5,Key)
	].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private methods

spaces(Data) ->
	length(binary:matches(Data,<<" ">>)).

numbers([],Acc) ->
	Acc;
numbers([H|L],Acc) when H >= $0, H =< $9 ->
	numbers(L, lists:append(Acc,[H]));
numbers([_|L],Acc) ->
	numbers(L, Acc).
	
numbers(Data) ->
	numbers(binary:bin_to_list(Data),[]).

dispatch(Pid, _Socket, _Opcode, Data) ->
	Payload = iolist_to_binary(Data),			%% construct a binary to pass back
	Pid ! { message, Payload }.				%% send the message

decode(Data,Acc) when is_binary(Data) ->			%% this guard is so we can willy nilly call decode with either a list or binary
	decode(binary:bin_to_list(Data),Acc);
decode([] = Data,Acc) when is_list(Data) ->			%% we reached the end of the data, but did not encounter end of frame
	{ [], Acc };
decode([0|L] = Data, _Acc) when is_list(Data) ->		%% start of new frame, old data as it is invalidly framed, discard Accumulator
	decode(L,[]);							
decode([255|L] = Data,Acc) when is_list(Data) ->		%% encountered the end of a frame
	{ lists:reverse(Acc), L };				%% return the prcoessed frame and leave the remainder.
decode([X|L] = Data, Acc) when is_list(Data) ->
	decode(L,[X|Acc]).					%% accumulate the frame contents
	
unframe(Pid,Socket,Payload,Data) ->
	{ Frame, Remainder } = decode(Data, Payload),		%% start with the previous accumulator state, and start decoding new data
	case Frame of 
		[] -> 
			Remainder;				%% no end of frame was found, just return the current accumulator output
		_ -> 
			dispatch(Pid,Socket,1,Frame),
			unframe(Pid,Socket,[],Remainder)
	end.
	
