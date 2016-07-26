printing
printOn: aStream
	self printNameOn: aStream.
	aStream
		nextPutAll: ' runs: ';
		print: runs;
		nextPutAll: ' values: ';
		print: values