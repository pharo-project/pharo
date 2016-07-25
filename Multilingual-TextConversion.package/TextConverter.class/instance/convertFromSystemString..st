conversion
convertFromSystemString: aString

	| readStream writeStream |
	readStream := aString readStream.
	writeStream := String new writeStream.

	[readStream atEnd] whileFalse: [
		writeStream nextPut: (self nextFromStream: readStream)].
	^writeStream contents
