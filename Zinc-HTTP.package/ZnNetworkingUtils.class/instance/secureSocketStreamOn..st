public
secureSocketStreamOn: socket
	| stream |
	stream := self secureSocketStreamClass on: socket.
	self setSocketStreamParameters: stream.  
	^ stream