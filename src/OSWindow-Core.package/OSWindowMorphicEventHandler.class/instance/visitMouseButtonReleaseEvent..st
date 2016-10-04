visiting
visitMouseButtonReleaseEvent: anEvent
	anEvent isWheel ifTrue: [ ^ nil ].

	^ MouseButtonEvent new
		setType: #mouseUp
		position: anEvent position 
		which: (self convertButton: anEvent button)
		buttons: (self convertModifiers: anEvent modifiers)
		hand: self activeHand
		stamp: Time millisecondClockValue