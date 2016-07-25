utilities
assertValues: anArray
	| values |
	values := collection collect: [ :each | each value ].
	self 
		assert: anArray asArray = values asArray
		description: ['Expected: ' , anArray asArray printString , 
			', but got ' , values asArray printString]