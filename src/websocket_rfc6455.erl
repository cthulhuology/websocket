-module(websocket_rfc6455).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2012,2013,2016 David J. Goehrig"/utf8>>).
-export([ handshake/2, frame/1, frame/2, frame/3, unframe/4, handler/2 ]).

frame(Data) when is_binary(Data) ->
	frame(Data,1).	%% 1 == text
	
frame(Data,Opcode) when is_binary(Data) ->
	frame(Data,Opcode, false ).		%% no masking

frame(Data,Opcode,Masked) when is_binary(Data) ->
	Len = iolist_size(Data),
	case Masked of	
		true -> 
			Mask = crypto:rand_bytes(4),
			framed(mask(Data,Mask), Opcode, Mask, Len);
		_ -> 
			framed(Data, Opcode, Len)
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


%%  Mask the data as a client
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

%% remove a mask form the data from the client
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


% wait for data
unframe(Pid,Socket,{ wait, Data },Payloads) ->
	receive 
		NewData when is_binary(NewData) ->
			unframe(Pid,Socket,{ parse, <<Data/binary,NewData/binary>> },Payloads);
		Msg ->
			io:format("unknown message ~p~n", [ Msg ]),
			unframe(Pid,Socket,wait,Payloads)
	end;

unframe(Pid,Socket, { parse, <<>> }, Payloads) ->
	unframe(Pid,Socket,{ wait, <<>> },Payloads);

% mask bit, and extract Finished bit, Opcode, and PayLen
unframe(Pid,Socket,{ parse, <<F:1,_R1:1,_R2:1,_R3:1,Opcode:4,1:1,PayLen:7,Data/binary>>}, Payloads) ->
	unframe(Pid,Socket,{ length, F, Opcode, masked, PayLen, Data }, Payloads);

% unmaked bit and extract Finished bit, Opcode, and PayLen
unframe(Pid,Socket,{ parse, <<F:1,_R1:1,_R2:1,_R3:1,Opcode:4,0:1,PayLen:7,Data/binary>>}, Payloads) ->
	unframe(Pid,Socket,{ length, F, Opcode, unmasked, PayLen, Data }, Payloads);

% wait for more data, because we can't parse a frame
unframe(Pid,Socket,{ parse, Data },Payloads) ->
	io:format("unframe error ~p~n", [ Data ]),
	unframe(Pid,Socket, { wait, Data } ,Payloads);

% 16bit length for masked
unframe(Pid,Socket,{ length, F, Opcode, masked, 126, <<Length:16,Remainder/binary>> = Data }, Payloads) when byte_size(Data) > 1 ->
	unframe(Pid,Socket,{ unmask, F, Opcode, masked, Length, Remainder }, Payloads);

% 64bit length for masked
unframe(Pid,Socket,{ length, F, Opcode, masked, 127, <<Length:64,Remainder/binary>> = Data }, Payloads) when byte_size(Data) > 3 ->
	unframe(Pid,Socket,{ unmask, F, Opcode, Length, Remainder }, Payloads);

% 7bit length for masked
unframe(Pid,Socket,{ length, F, Opcode, masked, Length, Data }, Payloads) ->
	unframe(Pid,Socket,{ unmask, F, Opcode, masked, Length, Data }, Payloads);

% 16bit length for unmasked
unframe(Pid,Socket,{ length, F, Opcode, unmasked, 126, <<Length:16,Remainder/binary>> = Data }, Payloads) when byte_size(Data) > 1 ->
	unframe(Pid,Socket,{ unpack, F, Opcode, unmasked, Length, Remainder }, Payloads);

% 64bit length for unmasked
unframe(Pid,Socket,{ length, F, Opcode, unmasked, 127, <<Length:64,Remainder/binary>> = Data }, Payloads) when byte_size(Data) > 3 ->
	unframe(Pid,Socket,{ unpack, F, Opcode, unmasked, Length, Remainder }, Payloads);

% 7bit length for unmasked
unframe(Pid,Socket,{ length, F, Opcode, unmasked, Length, Data }, Payloads) ->
	unframe(Pid,Socket,{ unpack, F, Opcode, unmasked, Length, Data }, Payloads);

% extract 32bit mask
unframe(Pid,Socket,{ unmask, F, Opcode, masked, Length, <<Mask:4/binary,Remainder/binary>> = Data }, Payloads) when byte_size(Data) > 3  ->
	unframe(Pid,Socket,{ unpack, F, Opcode, Mask, Length, Remainder }, Payloads);

% payload for unmasked
unframe(Pid,Socket,{ unpack, F, Opcode, unmasked, Length, Data },Payloads) when byte_size(Data) >= Length ->
	<<Payload:Length/binary,Remainder/binary>> = Data,
	unframe(Pid,Socket,{ deliver, F, Opcode, unmasked, Length, Remainder }, [ Payload | Payloads]);

% payload for masked,and decode
unframe(Pid,Socket,{ unpack, F, Opcode, Mask, Length, Data }, Payloads) when byte_size(Data) >= Length ->
	<<Payload:Length/binary,Remainder/binary>> = Data,
	unframe(Pid,Socket,{ deliver, F, Opcode, Mask, Length, Remainder }, [ unmask(Payload,Mask) | Payloads ]);

% continutation frame
unframe(Pid,Socket,{ deliver, 0, _Opcode, _Mask, _Length, Data }, Payloads) ->
	unframe(Pid,Socket,{ parse, Data }, Payloads);

% finished frame
unframe(Pid,Socket,{ deliver, 1, Opcode, _Mask, _Length, Data }, Payloads) ->
	dispatch(Pid,Socket,Opcode,Payloads),
	unframe(Pid,Socket,{ parse, Data },[]);

%% wait for new data, so we can advance to the next stage
unframe(Pid,Socket,{ Stage, F, Opcode, Mask, Length, Data }, Payloads) ->
	receive 
		NewData when is_binary(NewData) ->
			unframe(Pid,Socket, { Stage, F, Opcode, Mask, Length, <<Data/binary,NewData/binary>>}, Payloads);
		Msg ->
			io:format("unknown message ~p~n", [ Msg ]),
			unframe(Pid,Socket,{ Stage, F, Opcode, Mask, Length, Data }, Payloads)
	end.

%% returns a handler process
handler(Pid,Socket) ->
	spawn_link(?MODULE,unframe,[Pid,Socket,{ wait, <<>> },[]]).
