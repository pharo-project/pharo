instance creation
readHeaderFrom: stream
	^ self new
		readHeaderFrom: stream;
		yourself