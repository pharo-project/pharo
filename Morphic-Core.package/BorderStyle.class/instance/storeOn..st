printing
storeOn: aStream
	"Store a reconstructable representation of the
	receiver on the given stream."

	aStream
		nextPutAll: '(';
		nextPutAll: self class name;
		nextPutAll: ' width: ';
		print: self width;
		nextPutAll: ' color: ';
		print: self color;
		nextPutAll: ')'