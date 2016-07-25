adding/removing
addItemMorph: aMorph before: anotherMorph

	self addBrickBackNoLayout: aMorph before: anotherMorph.
	self setBoundsOf: aMorph before: anotherMorph.
	(self isPartiallyVisible: aMorph) ifFalse: [ self removeBrickNoLayout: aMorph.  ^ nil ].
	
	aMorph brickBounds top < 0
		ifTrue: [ self moveAllSubbricksBy: aMorph brickBounds top negated ]
		ifFalse: [ (self getIndex: aMorph) = 1 ifTrue: [
				self moveAllSubbricksBy: aMorph brickBounds top negated ] ].
	
	^ aMorph