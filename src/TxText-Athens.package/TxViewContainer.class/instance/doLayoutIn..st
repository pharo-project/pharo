layout
doLayoutIn: layoutBounds
	| box priorBounds processedBounds |
	priorBounds := self outerBounds.
	processedBounds := layoutBounds.
	decorators do: [:morph |
		processedBounds := morph computeBoundsIn: processedBounds ].
	
	
	view bounds: processedBounds.
	
	fullBounds := bounds.
	
	box := self outerBounds.
	box = priorBounds 
		ifFalse: [self invalidRect: (priorBounds quickMerge: box)]