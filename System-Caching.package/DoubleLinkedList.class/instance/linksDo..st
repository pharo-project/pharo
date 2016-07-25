enumerating
linksDo: block
	"Execute block for each of the links that I hold internally."
	
	| current |
	current := head.
	[ current isNil ] 
		whileFalse: [ 
	 		block value: current.
			current := current nextLink ]