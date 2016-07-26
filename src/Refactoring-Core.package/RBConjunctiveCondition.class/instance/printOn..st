printing
printOn: aStream 
	aStream nextPutAll: left asString;
		nextPutAll: ' & ';
		nextPutAll: right asString
