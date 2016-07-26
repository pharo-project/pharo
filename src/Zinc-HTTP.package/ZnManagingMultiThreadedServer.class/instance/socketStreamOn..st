private
socketStreamOn: socket
	| socketStream |
	socketStream := super socketStreamOn: socket.
	self lock critical: [ 
		self connections add: socketStream ].
	^ socketStream