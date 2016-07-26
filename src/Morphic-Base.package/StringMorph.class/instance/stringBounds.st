accessing
stringBounds

	| bnd gap |
	
	bnd := self bounds.
	gap := self layoutInset.
	
	^ (bnd topLeft + gap) corner: (bnd bottomRight + gap)