visiting
visitMouseButtonPressEvent: anEvent
	anEvent isWheel ifTrue: [ 
		^ MouseWheelEvent new 
			setType: #mouseWheel
			position: anEvent position
			direction: anEvent wheelDirection
			buttons: (self convertModifiers: anEvent modifiers)
			hand: self activeHand
			stamp: Time millisecondClockValue ].
			
	^ MouseButtonEvent new
		setType: #mouseDown 
		position: anEvent position 
		which: (self convertButton: anEvent button)
		buttons: (self convertModifiers: anEvent modifiers) | (self convertButton: anEvent button)
		hand: self activeHand
		stamp: Time millisecondClockValue