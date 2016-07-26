printing
printOn: aStream
	aStream 
		nextPutAll: self class name;
		nextPut:$(;
		print: start;
		nextPutAll:' - ';
		print: via;
		nextPutAll:' - ';
		print: end;
		nextPut:$)