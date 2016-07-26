tests
testAt
	| externalArray |
	
	externalArray := FFIExternalArray newType: 'int' size: 10.
	self assert: externalArray size equals: 10.
	1 to: 10 do: [ :index | self assert: (externalArray at: index) equals: 0 ]
