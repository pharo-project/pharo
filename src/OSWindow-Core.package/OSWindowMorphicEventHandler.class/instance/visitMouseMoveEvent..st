visiting
visitMouseMoveEvent: anEvent
	| oldPos |
	oldPos := morphicWorld activeHand ifNil: [ 0@0 ] ifNotNil: [:hand | hand position ].

	^ MouseMoveEvent basicNew
		setType: #mouseMove 
		startPoint: oldPos
		endPoint: anEvent position 
		trail: { oldPos. anEvent position }
		buttons: (self convertModifiers: anEvent modifiers)
		hand: self activeHand
		stamp: Time millisecondClockValue