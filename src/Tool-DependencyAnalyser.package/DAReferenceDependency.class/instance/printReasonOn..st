printing
printReasonOn: aStream
	aStream
		nextPutAll: self theClass name;
		nextPutAll: '>>';
		print: self selector;
		nextPutAll: ' references ';
		print: self reference