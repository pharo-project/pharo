private-structure
privateAddIntoWorld: aBrick

	self world == aBrick world
		ifFalse: [ self privateRemoveOutWorld: aBrick ].
	aBrick intoWorld: self world
	