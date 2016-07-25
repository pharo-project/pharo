private
serverSocketOn: port
	| socket |
	(socket := Socket newTCP)
		setOption: 'TCP_NODELAY' value: 1;
		setOption: 'SO_SNDBUF' value: self socketBufferSize;
		setOption: 'SO_RCVBUF' value: self socketBufferSize .
	socket listenOn: port backlogSize: self listenBacklogSize.
	socket isValid
		ifFalse: [ self error: 'Cannot create socket on port ', port printString ].
	^ socket