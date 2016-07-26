private
socketStreamOn: socket
	| stream |
	stream := super socketStreamOn: socket.
	self setReferenceSocketStreamOptions: stream.
	^ stream	