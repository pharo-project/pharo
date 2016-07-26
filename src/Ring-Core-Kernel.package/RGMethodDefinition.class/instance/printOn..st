printing
printOn: aStream 

	self parentName ifNotNil: [
		aStream nextPutAll: self parentName;
				  nextPutAll: '>>' ].
	aStream print: self selector