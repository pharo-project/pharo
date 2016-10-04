initialization
initialize
	super initialize.
	self color: Color veryLightGray.
	isResizeable := true.
	self extent: 640@480.
	worldState := OSWindowWorldState new.
	self addHand: HandMorph new.
	