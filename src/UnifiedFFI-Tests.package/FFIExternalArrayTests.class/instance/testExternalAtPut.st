tests
testExternalAtPut
	| externalArray |
	
	[	
		externalArray := FFIExternalArray externalNewType: 'int' size: 1.
		externalArray at: 1 put: 42.
		self assert: (externalArray at: 1) equals: 42 ]
	ensure: [ 
		externalArray free ]