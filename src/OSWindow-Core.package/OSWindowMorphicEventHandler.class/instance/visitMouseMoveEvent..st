visiting
visitMouseMoveEvent: anEvent
	| oldPos |
	oldPos := morphicWorld activeHand position.

	^ MouseMoveEvent basicNew
		setType: #mouseMove 
		startPoint: oldPos
		endPoint: anEvent position 
		trail: { oldPos. anEvent position }
		buttons: (self convertModifiers: anEvent modifiers)
		hand: self activeHand
		stamp: Time millisecondClockValue