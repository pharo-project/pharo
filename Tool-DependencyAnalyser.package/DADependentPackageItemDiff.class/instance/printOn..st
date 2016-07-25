printing
printOn: aStream 
	aStream 
		nextPutAll: content packageName;
		nextPutAll: ' Parent : ';
		nextPutAll: self parent packageName