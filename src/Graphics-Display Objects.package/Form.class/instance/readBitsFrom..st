filein/out
readBitsFrom: aBinaryStream
	
	bits := Bitmap newFromStream: aBinaryStream.
	bits size = self bitsSize ifFalse: [self error: 'wrong bitmap size; bad Form file?'].
	^ self
