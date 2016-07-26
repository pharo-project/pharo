private out
flushBytes: bytes startingAt: offset count: count
	"Ask the socket to write count bytes starting from offset. Wait. Fail if not successful"
	
	| writeCount |
	self isConnected ifFalse: [ ConnectionClosed signal: 'Cannot write data' ].  
	writeCount := 0.
	[ | written |
		written := self socketSendData: bytes startingAt: offset + writeCount count: count - writeCount.
		(writeCount := writeCount + written) = count ] whileFalse: [
		self socketWaitForSendDone
			ifFalse: [ ConnectionTimedOut signal: 'Data send timed out.' ] ]