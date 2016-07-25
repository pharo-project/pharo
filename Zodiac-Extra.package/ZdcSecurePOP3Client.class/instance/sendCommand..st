private protocol
sendCommand: aString
	self logVerbose: aString.
	self stream 
		nextPutAll: aString;
		nextPutAll: #[13 10];
		flush
