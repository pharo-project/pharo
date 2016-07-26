conversion
convertToSystemString: aString

	| writeStream |
	writeStream := String new writeStream.
	self nextPutAll: aString toStream: writeStream.
	self emitSequenceToResetStateIfNeededOn: writeStream.
	^writeStream contents