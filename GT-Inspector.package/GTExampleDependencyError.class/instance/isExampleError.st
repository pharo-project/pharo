testing
isExampleError
	^ self exception 
		ifNotNil: [ self exception isExampleError ]
		ifNil: [ super isExampleError ]