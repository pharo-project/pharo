visiting
visitJoyButtonDownEvent: buttonEvent
	buttonEvent isPreferredEvent ifFalse: [ ^ self ].
	
	buttonEvent isButtonX ifTrue: [ 
		example shoot.
	].

	buttonEvent isButtonStart ifTrue: [ 
		example reset.
	].
