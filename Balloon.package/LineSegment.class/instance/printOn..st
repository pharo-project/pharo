printing
printOn: aStream
	"Print the receiver on aStream"
	aStream 
		nextPutAll: self class name;
		nextPutAll:' from: ';
		print: start;
		nextPutAll: ' to: ';
		print: end;
		space.