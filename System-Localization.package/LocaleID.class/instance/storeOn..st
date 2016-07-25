printing
storeOn: aStream 
	aStream nextPut: $(.
	aStream nextPutAll: self class name.
	aStream nextPutAll: ' isoString: '.
	aStream nextPutAll: '''' , self printString , ''''.
	aStream nextPut: $).
