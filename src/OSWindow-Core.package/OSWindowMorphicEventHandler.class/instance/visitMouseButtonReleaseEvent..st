visiting
visitMouseButtonReleaseEvent: anEvent
	anEvent button > 3 ifTrue: [ ^ nil ].

	^ MouseButtonEvent basicNew
		setType: #mouseUp
		position: anEvent position 
		which: (self convertButton: anEvent button)
		buttons: (self convertModifiers: anEvent modifiers)
		hand: self activeHand
		stamp: Time millisecondClockValue