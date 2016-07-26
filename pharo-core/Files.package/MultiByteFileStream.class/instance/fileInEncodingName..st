accessing
fileInEncodingName: aString

	self converter: (TextConverter newForEncoding: aString).
	super fileIn.
