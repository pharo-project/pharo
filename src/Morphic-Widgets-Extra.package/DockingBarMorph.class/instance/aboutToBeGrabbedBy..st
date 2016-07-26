dropping/grabbing
aboutToBeGrabbedBy: aHand 
	"The morph is about to be grabbed, make it float"
	self beFloating.
	self updateBounds.
	self updateColor.
	(self bounds containsPoint: aHand position)
		ifFalse: [self center: aHand position].
