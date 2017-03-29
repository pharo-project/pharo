accessing
triggerBrick: aBrick
	"Add a given interractable brick as my trigger brick.
	Trigger brick is a visual element that should be located exactly above original
	anchor brick and should have the same (visual) content.
	Note: background of trigger brick may differ from background of anchor brick.
	aBrick must not be nil"
	
	self
		assert: [ aBrick isNotNil ]
		description: [ 'Trigger brick must not be nil' ].

	"If we already have a trigger brick, replace it with new one"
	triggerBrick ifNotNil: [
		self removeBrick: triggerBrick ].

	triggerBrick := aBrick.
	self addBrickBack: self triggerBrick