dropping/grabbing
aboutToBeGrabbedBy: aHand
	self grabbedAllowed
		ifTrue: [ ^ super aboutToBeGrabbedBy: aHand ].
	^ nil