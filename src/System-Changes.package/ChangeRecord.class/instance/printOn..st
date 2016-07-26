printing
printOn: aStream

	aStream 
		nextPutAll: self type printString.
	self stamp ifNotNil: [ aStream nextPutAll: self stamp ]