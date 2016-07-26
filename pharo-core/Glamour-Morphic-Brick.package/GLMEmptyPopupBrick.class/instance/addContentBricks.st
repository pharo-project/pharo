accessing
addContentBricks
	
	self onCreated.
	contentLogic ifNotNil: [
		self contentBrick addAllBricks: ((contentLogic brickValue: self withEnoughArguments: { self triggerBrick }) asOrderedCollection select: #isNotNil) ].
	self announce: #onCreated event: self triggerBrick