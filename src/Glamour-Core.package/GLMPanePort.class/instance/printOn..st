accessing
printOn: aStream 
	 
	aStream 
		nextPutAll: 'Port (pane='; 
		nextPutAll: self pane name printString; 
		nextPutAll: ' name='; 
		nextPutAll: self name printString; 
		nextPutAll: ' value='; 
		nextPutAll: self value printString; 
		nextPut: $)