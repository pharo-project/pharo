conversion
nextPutAll: aString toStream: aStream
	
	self checkBOMOn: aStream.
	^super nextPutAll: aString toStream: aStream