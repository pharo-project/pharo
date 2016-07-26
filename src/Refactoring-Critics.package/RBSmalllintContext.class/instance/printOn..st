printing
printOn: aStream
	super printOn: aStream.
	self selectedClass isNil ifFalse: [ 
		aStream nextPut: $ ; nextPutAll: self selectedClass name.
		self selector isNil ifFalse: [ 
			aStream nextPutAll: '>>'; print: self selector ] ]