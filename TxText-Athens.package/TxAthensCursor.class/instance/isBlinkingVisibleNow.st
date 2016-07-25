blinking support
isBlinkingVisibleNow
	| tick |
	
	tick := Time millisecondClockValue - lastChangeTick.
	
	tick < 0 ifTrue: [ 
		"rare case when tick counter wrapped"
		lastChangeTick := Time millisecondClockValue.
		tick := 0 ].
	
	^ (tick // self blinkIntervalMs) even
	