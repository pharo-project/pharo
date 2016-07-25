printing
printOn: aStream 
	name isNil
		ifTrue: [super printOn: aStream]
		ifFalse: [aStream nextPutAll: name]