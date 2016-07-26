read, write, position
peek

	^ nextChar isNil
		ifTrue: [reader closed
				ifFalse: [nextChar := reader next]]
		ifFalse: [nextChar]