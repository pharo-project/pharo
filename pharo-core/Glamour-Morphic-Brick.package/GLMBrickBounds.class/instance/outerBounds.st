accessing - bounds
outerBounds

	^ outerBounds ifNil: [ self updateCache. outerBounds ]