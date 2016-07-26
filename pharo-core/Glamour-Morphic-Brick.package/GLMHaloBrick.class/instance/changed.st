updating
changed

	^ fullBounds 
		ifNil: [ self invalidRect: self privateFullBounds]
		ifNotNil: [ self invalidRect: fullBounds ]