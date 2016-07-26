instance creation
readFrom: stream
	^ self new
		readFrom: stream;
		yourself