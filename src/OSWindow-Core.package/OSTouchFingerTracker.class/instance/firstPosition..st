moves
firstPosition: anObject
	move removeAll.
	firstPosition := anObject.
	self addToMove: firstPosition.