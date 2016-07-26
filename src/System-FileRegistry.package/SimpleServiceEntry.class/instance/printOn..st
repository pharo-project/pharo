printing
printOn: aStream

	aStream nextPutAll: self class name; nextPutAll: ': ('.
	self provider notNil
		ifTrue: [aStream nextPutAll: provider printString].
	aStream nextPutAll: ' --- '. 
	self selector notNil
		ifTrue: [aStream nextPutAll: selector asString].
	aStream nextPut: $)

