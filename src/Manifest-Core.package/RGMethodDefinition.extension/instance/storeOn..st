*Manifest-Core
storeOn: aStream
	aStream nextPut: $(.
	aStream nextPutAll: self class asString.
	aStream nextPutAll: ' className: '''.
	aStream nextPutAll: self parentName.
	aStream nextPutAll: ''' selector: '.
	aStream nextPutAll: self selector printString.
	aStream nextPutAll: ' isMetaSide: '.
	aStream nextPutAll: self isMetaSide printString.
	aStream nextPut: $)