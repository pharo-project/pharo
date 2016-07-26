layout
computeBounds
	[ self doLayoutIn: self layoutBounds ]
		on: Error
		do: [ :ex | 
			"This should do it unless you don't screw up the bounds"
			fullBounds := bounds.
			ex pass ]