visiting
visitMouseWheelEvent: anEvent
	| vertical |
	vertical := anEvent scrollVertical.
	vertical = 0 ifTrue: [ ^ nil ].
	
	^ MouseWheelEvent new 
		setType: #mouseWheel
		position: anEvent position
		direction: (vertical > 0 ifTrue: [Character arrowUp] ifFalse: [Character arrowDown])
		buttons: (self convertModifiers: anEvent modifiers)
		hand: self activeHand
		stamp: Time millisecondClockValue