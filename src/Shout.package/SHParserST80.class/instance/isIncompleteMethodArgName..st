identifier testing
isIncompleteMethodArgName: aString 
	^ (arguments
		at: 0
		ifAbsent: [#()])
		anySatisfy: [:each | each beginsWith: aString]