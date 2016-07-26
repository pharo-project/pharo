testing
isFitsInParent

	^ self owner
		ifNil: [ self brickBounds containsRect: (self outerBounds origin extent: self textSize) ]
		ifNotNil: [ self owner brickBounds width >= self textSize x ]