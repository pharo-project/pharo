private socket
socketReceiveDataInto: bytes startingAt: offset count: count
	^ socket 
		primSocket: socket socketHandle 
		receiveDataInto: bytes
		startingAt: offset
		count: count