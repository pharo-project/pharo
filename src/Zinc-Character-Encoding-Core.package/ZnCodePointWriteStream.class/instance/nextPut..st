accessing
nextPut: object
	self encoder 
		nextPutCodePoint: object 
		toStream: stream.
	^ object