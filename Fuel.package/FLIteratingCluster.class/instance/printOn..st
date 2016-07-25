printing
printOn: aStream

	self printNameOn: aStream.
	aStream nextPutAll: '->'.
	objects printElementsOn: aStream