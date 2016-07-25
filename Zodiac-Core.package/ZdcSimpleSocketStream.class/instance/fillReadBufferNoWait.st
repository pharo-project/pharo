private in
fillReadBufferNoWait
	"Ask the socket to fill the read buffer with data. Do not wait for data."
	
	| readCount |
	readBuffer compact.
	readCount := self 
		fillBytes: readBuffer buffer 
		startingAt: readBuffer freeSpaceStart
		count: readBuffer availableForWriting .
	readBuffer advanceWritePointer: readCount.
	^ readCount