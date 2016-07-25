enumerating
reverseLinksDo: block
	"Execute block for each of the links that I hold internally, in reverse order."

	| current |
	current := tail.
	[ current isNil ] 
		whileFalse: [ 
	 		block value: current.
			current := current previousLink ]