printing
printOn: aStream

	analyzedClassName isNil
		ifFalse: [aStream nextPutAll: analyzedClassName asString]. 
	aStream nextPutAll: ' ('.
	codeSize isNil
		ifFalse: [aStream nextPutAll: 'code size: ' ;  nextPutAll: codeSize asString]. 
	instanceCount isNil
		ifFalse: [aStream nextPutAll: ' instance count: ' ;  nextPutAll: instanceCount asString]. 
	spaceForInstances isNil
		ifFalse: [aStream nextPutAll: ' space for instances: ' ;  nextPutAll: spaceForInstances asString]. 
	aStream nextPut: $).
	