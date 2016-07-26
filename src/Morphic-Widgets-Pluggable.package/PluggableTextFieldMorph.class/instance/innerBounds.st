geometry
innerBounds
	| inb |
	inb := super innerBounds.
	endRow 
		ifNotNil: [inb := inb withRight: (inb right - endRow fullBounds width)].
	^ inb