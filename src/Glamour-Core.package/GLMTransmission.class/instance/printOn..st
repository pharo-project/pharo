transmitting
printOn: aStream 
	 
	aStream 
		nextPutAll: self class name;
		nextPutAll: ' (origins='; 
		nextPutAll: self origins printString; 
		nextPutAll: ' destination='; 
		nextPutAll: self destination printString; 
		nextPut: $)
	 
	