lint
atIfAbsent
	^ Smalltalk at: #MyTest
		ifAbsent: 
			[| collection |
			collection := #().
			Smalltalk at: #MyTest put: collection]