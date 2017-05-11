accessing
brickRoot: aBrick

	self brickRoot ifNotNil: [
		self removeMorph: self brickRoot.
		brickRoot := nil ].
	
	brickRoot := aBrick.
	self addMorphBack: brickRoot.
	brickRoot onThemerChanged