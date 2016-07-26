printing
printOn: aStream
	aStream nextPutAll: 'a NodeModel with '.
	aStream nextPutAll: self item printString.