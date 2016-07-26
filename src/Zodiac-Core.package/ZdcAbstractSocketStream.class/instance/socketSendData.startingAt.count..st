private socket
socketSendData: bytes startingAt: offset count: count
	^ socket 
		sendSomeData: bytes 
		startIndex: offset 
		count: count 