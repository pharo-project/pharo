stream out
sendCommand: aString
	self outStream
		nextPutAll: aString;
		nextPutAll: String crlf.