accessing
nextPut: object
	self encoder 
		nextPut: object 
		toStream: stream.
	^ object