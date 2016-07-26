printing
printOn: aStream
	aStream 
		nextPutAll: self name printString;
		nextPutAll: ' on ';
		nextPutAll: self shortcut printString;
		nextPutAll: ' do ' ;
		nextPutAll: self action printString;
		cr.    