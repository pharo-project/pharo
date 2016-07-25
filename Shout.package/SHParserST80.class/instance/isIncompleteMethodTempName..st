identifier testing
isIncompleteMethodTempName: aString 
	^ (temporaries
		at: 0
		ifAbsent: [#()])
		anySatisfy: [:each | each beginsWith: aString]