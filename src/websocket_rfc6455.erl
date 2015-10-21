-module(websocket_rfc6455).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2012,2013 David J. Goehrig"/utf8>>).
-export([ handshake/2, frame/1, frame/2, frame/3, unframe/4, handle/4 ]).

frame(Data) when is_binary(Data) ->
	frame(Data,1).	%% 1 == text
	
frame(Data,Opcode) when is_binary(Data) ->
	frame(Data,Opcode, false ).		%% no masking

frame(Data,Opcode,Masked) when is_binary(Data) ->
	Len = iolist_size(Data),
	case Masked of	
		true -> 
			Mask = crypo:rand_bytes(4),
			framed(mask(Data,Mask), Opcode, Mask, Len);
		_ -> 
			framed(Data, Opcode, Len)
	end.

%% handle take in a message and return back a list 
handle(Pid,Socket,Message,OldData) ->
	case Message of
		{tcp, Socket, NewData} ->
			unframe(Pid,Socket,OldData,NewData);
		{tcp_closed, Socket } ->
			Pid ! { close, Socket },
			OldData;
		_ ->
			io:format("Unexpected message: ~p~n", [ Message ]),
			OldData
	end.
	

%% Calcuate an rfc6455 handshake
handshake(Headers,_Data) ->
	Key = proplists:get_value(<<"Sec-WebSocket-Key">>,Headers),
	Shake = <<Key/binary,"258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>, %% 25.. is magic
	Crypt = crypto:hash(sha,Shake),
	Secret = base64:encode(Crypt),
	[
		"HTTP/1.1 101 Switching Protocols\r\n",
		"Upgrade: websocket\r\n",
		"Connection: Upgrade\r\n",
		"Sec-WebSocket-Accept: ", Secret, "\r\n",
		"\r\n"
	].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private methods

%% handle protocol logic like ping/pong etc.	
dispatch(Pid, Socket, Opcode, Data ) ->
	Payload = iolist_to_binary(lists:reverse(Data)),
	case Opcode of
		0 -> Pid ! { message, Payload };	%% continuation
		1 -> Pid ! { message, Payload };	%% text
		2 -> Pid ! { message, Payload };	%% binary
		8 -> Pid ! { close, Socket }; 		%% close
		9 -> Pid ! { ping, Socket };		%% ping
		10 -> Pid ! { pong, Socket };		%% pong
		Any -> Pid ! { unknown, Any }		%% unknown
	end.

%% Frame a Datagram with the appropriate Opcode, Length, and Mask

framed(Data,Opcode,Len) when Len < 126 ->
	Res = <<1:1,0:1,0:1,0:1,Opcode:4,0:1,Len:7,Data/binary>>,
	Res;
framed(Data,Opcode,Len) when Len < 65536 ->
	Res = <<1:1,0:1,0:1,0:1,Opcode:4,0:1,126:7,Len:16,Data/binary>>,
	Res;
framed(Data,Opcode,Len) ->
	Res = <<1:1,0:1,0:1,0:1,Opcode:4,0:1,127:7,Len:64,Data/binary>>,
	Res.

framed(Data,Opcode,Mask,Len) when Len < 126 ->
	Res = <<1:1,0:1,0:1,0:1,Opcode:4,1:1,Len:7,Mask:4/binary,Data/binary>>,
	Res;
framed(Data,Opcode,Mask,Len) when Len < 65536 ->
	Res = <<1:1,0:1,0:1,0:1,Opcode:4,1:1,126:7,Len:16,Mask:4/binary,Data/binary>>,
	Res;
framed(Data,Opcode,Mask,Len) ->
	Res = <<1:1,0:1,0:1,0:1,Opcode:4,1:1,127:7,Len:64,Mask:4/binary,Data/binary>>,
	Res.


%%  Mask 
mask(<<>>,_Mask,_I,Acc) ->
	binary:list_to_bin(lists:reverse(Acc));
mask(<<D:8,Data/binary>>,<<M1:8,M2:8,M3:8,M4:8>> = Mask, I, Acc) ->
	C = case I rem 4 of 
		0 -> D bxor M1;
		1 -> D bxor M2;
		2 -> D bxor M3;
		3 -> D bxor M4
	end,
	mask(Data,Mask,I+1, [ C | Acc]).

mask(Data,Mask) ->
	mask(Data,Mask,0,[]).

unmask(<<>>,_Mask,_I,Acc) ->
	lists:reverse(Acc);
unmask(<<D:8,Data/binary>>,<<M1:8,M2:8,M3:8,M4:8>> = Mask,I,Acc) ->
	C = case I rem 4 of
		0 -> [ D bxor M1 | Acc ];
		1 -> [ D bxor M2 | Acc ];
		2 -> [ D bxor M3 | Acc ];
		3 -> [ D bxor M4 | Acc ]
	end,
	unmask(Data,Mask,I+1,C).

unmask(Data,<<"">>) when is_binary(Data) ->
	Data;
unmask(Data,Mask) when is_binary(Data) ->
	unmask(Data,Mask,0,[]).

%% Peek extracts the header data out of the frame
peek(<<F:1,_R1:1,_R2:1,_R3:1,Opcode:4,1:1,PayLen:7,Data/binary>>) ->	%% Mask bit set
	case PayLen of
		126 -> <<Length:16,Mask:4/binary,Remainder/binary>> = Data;
		127 -> <<Length:64,Mask:4/binary,Remainder/binary>> = Data;
		Length -> <<Mask:4/binary,Remainder/binary>> = Data
	end,
	{ F, Opcode, Length, Mask, Remainder };
peek(<<F:1,_R1:1,_R2:1,_R3:1,Opcode:4,0:1,PayLen:7,Data/binary>>) ->	%% No Mask bit set
	case PayLen of
		126 -> <<Length:16,Remainder/binary>> = Data;
		127 -> <<Length:64,Remainder/binary>> = Data;
		Length -> <<Remainder/binary>> = Data
	end,
	{ F, Opcode, Length, <<"">>, Remainder }.

%% applies the mask if any
decode(Data,Length,<<"">>) ->
	<<Payload:Length/binary,Remainder/binary>> = Data,
	{ Payload, Remainder };	
decode(Data,Length,Mask) ->		
	<<Payload:Length/binary,Remainder/binary>> = Data,
	{ unmask(Payload,Mask), Remainder }.


%% unframe 
unframe(_Pid,_Socket,[],<<>>)  ->						%% when we have no more data, reset socket state
	[];									%% an empty list indicates there was no previous frame.
unframe(Pid,Socket,[],NewData) when byte_size(NewData) > 2 ->
	{ Finished, Opcode, Length, Mask, Remainder } = peek(NewData),		%% we check to see if we have a valid frame header
	RemainderLength = iolist_size(Remainder),
	if (Length > RemainderLength) ->					%% but if we're short bytes for this header
		{ Finished, Opcode, Length, Mask, [], Remainder };		%% we save it for next time..
	(Finished =:= 1) -> 
		{ Payload, Rest } = decode(Remainder,Length,Mask),		%% If we found we got a complete frame w/ data, we decode it
		dispatch(Pid,Socket,Opcode,[Payload]),				%% and dispatch to execute the associated method on it
		unframe(Pid,Socket,[],Rest);					%% then process the remainder as the start of a new frame.
	%% we have a continuation frame....
	true ->								 	%% If we're not finished collecting frames but have a full frame
		{ Payload, Rest } = decode(Remainder,Length,Mask),		%% try to decode and then process the next frame
		unframe(Pid,Socket,{ Finished, Opcode, Length, Mask, [ Payload ], <<>> }, Rest)
	end;
unframe(_Pid,_Socket,OldData,<<>>) ->						%% we have no more data, Preserve the previous state...
	OldData;
unframe(Pid,Socket,OldData,NewData) ->						%% now we've seen some data, but haven't finished processing
	{ Finished, Opcode, Length, Mask, Payloads, Remainder } = OldData,	%% we may have Payload, Remainder, or Payload&Remainder
	Rest = <<Remainder/binary,NewData/binary>>,				%% we concatenate the new data onto the leftovers
	RestLength = iolist_size(Rest),
	if (Length > RestLength) ->
		{ Finished, Opcode, Length, Mask, Payloads, Rest };		%% we have more but not enough, save the aggregate for later
	(Finished =:= 1) ->
		{ Payload2, Rest2 } = decode(Rest,Length,Mask),			%% here we finished a frame so we decode that frame
		dispatch(Pid,Socket,Opcode, [ Payload2 | Payloads ]),		%% we conjoin the payload from the previous frames with the new data
		unframe(Pid,Socket,[],Rest2);					%% start processing the next frame if any exists
	%% we have a continuation frame....
	true ->										%% here we have a complete continuation frame so we
		{ Payload2, Rest2 } = decode(Rest,Length,Mask),				%% decode the current continuation frame and
		if byte_size(Rest2) > 2 ->
			{ Finished2, Opcode2, Length2, Mask2, _Remainder2 } = peek(Rest2),	%% attempt to decode the start of the next frame
			unframe(Pid,Socket, { Finished2, Opcode2, Length2, Mask2, [ Payload2 | Payloads ], <<>> }, Rest2 );
		true ->
			io:format("Invalid frame offset"),
			[]
		end
	end.
	
