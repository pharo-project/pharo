printing
printOn: aStream
	"Print me->ancestor"
	aStream
		print: element;
		nextPutAll: ' -> ';
		print: indent