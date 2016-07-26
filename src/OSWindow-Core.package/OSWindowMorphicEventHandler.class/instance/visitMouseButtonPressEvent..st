visiting
visitMouseButtonPressEvent: anEvent
	anEvent button > 3 ifTrue: [ 
		^ MouseWheelEvent new 
			setType: #mouseWheel
			position: anEvent position
			direction: (anEvent button = 4 ifTrue: [#up] ifFalse: [#down])
			buttons: (self convertModifiers: anEvent modifiers)
			hand: self activeHand
			stamp: Time millisecondClockValue ].
			
	^ MouseButtonEvent basicNew
		setType: #mouseDown 
		position: anEvent position 
		which: (self convertButton: anEvent button)
		buttons: (self convertModifiers: anEvent modifiers) | (self convertButton: anEvent button)
		hand: self activeHand
		stamp: Time millisecondClockValue