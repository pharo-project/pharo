public
socketStreamOn: socket
	| stream |
	stream := self socketStreamClass on: socket.
	self setSocketStreamParameters: stream.  
	^ stream