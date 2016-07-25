public
serverSocketOn: port
	| socket |
	socket := Socket newTCP.
	self setServerSocketOptions: socket.
	socket listenOn: port backlogSize: self class listenBacklogSize.
	socket isValid
		ifFalse: [ self error: 'Cannot create socket on port ', port printString ].
	^ socket