private
referenceSocketStreamOn: socket
	| stream |
	stream := SocketStream on: socket.
	self setReferenceSocketStreamOptions: stream.
	^ stream