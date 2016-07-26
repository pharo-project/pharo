tests
testAt
	| externalArray |
	
	externalArray := (FFITypeArray ofType: 'int' size: 10) new.
	self assert: externalArray size equals: 10.
	1 to: 10 do: [ :index | self assert: (externalArray at: index) equals: 0 ]
