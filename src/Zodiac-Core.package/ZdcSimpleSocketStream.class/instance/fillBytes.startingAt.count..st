private in
fillBytes: bytes startingAt: offset count: count
	"Ask the socket to read count elements into bytes starting at offset. Do not wait. Return read count."
	
	self isConnected ifFalse: [ ConnectionClosed signal: 'Cannot read data' ].  
	^ self 
		socketReceiveDataInto: bytes 
		startingAt: offset 
		count: count