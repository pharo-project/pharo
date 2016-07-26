enumerating
linksDo: aBlock
	| current |
	current := head.
	[ current isNil ] 
		whileFalse: [ 
	 		aBlock value: current.
			current := current nextLink ]