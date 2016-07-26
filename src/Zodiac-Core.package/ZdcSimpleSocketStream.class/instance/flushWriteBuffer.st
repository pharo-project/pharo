private out
flushWriteBuffer
	"Ask the socket to write all data in the write buffer. Fail if not successful"
	
	writeBuffer isEmpty ifTrue: [ ^ self ].  
	self 
		flushBytes: writeBuffer buffer
		startingAt: writeBuffer contentsStart
		count: writeBuffer availableForReading. 
	writeBuffer reset 