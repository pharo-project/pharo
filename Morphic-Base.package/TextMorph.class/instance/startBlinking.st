blinking
startBlinking
	self startStepping: #onBlinkCursor 
		at: Time millisecondClockValue 
		arguments: nil stepTime: 500.
	self resetBlinkCursor.
