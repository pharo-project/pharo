instance creation
on: writeStream bufferSize: aSize
	^ self basicNew
		initializeOn: writeStream 
		bufferSize: aSize;
		yourself