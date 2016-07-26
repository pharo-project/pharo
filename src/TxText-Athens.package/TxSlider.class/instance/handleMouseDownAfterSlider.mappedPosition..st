event handling
handleMouseDownAfterSlider: evt  mappedPosition: downPos

	self announcer announce: (TxSliderScrollForward source: self).

	self world 
		startStepping: self 
		at: Time millisecondClockValue
		selector: #animateScrollingForward: arguments: { downPos } stepTime: self scrollingDelay.
	