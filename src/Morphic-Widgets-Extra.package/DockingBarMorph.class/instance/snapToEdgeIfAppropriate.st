construction
snapToEdgeIfAppropriate
	(self owner isNil
			or: [self owner isHandMorph])
		ifTrue: [^ self].
	self updateBounds