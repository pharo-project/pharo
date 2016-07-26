printing
printOn: aStream
	super printOn: aStream.
	aStream nextPutAll: ' amount: '.
	amount printOn: aStream