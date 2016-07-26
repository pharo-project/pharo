printing
printOn: aStream 
	aStream
		nextPutAll: self displayClassName;
		nextPutAll: ' removeSelector: ';
		print: self selector;
		nextPut: $!