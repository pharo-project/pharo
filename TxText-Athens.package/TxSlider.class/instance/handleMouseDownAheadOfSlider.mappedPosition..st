event handling
handleMouseDownAheadOfSlider: evt mappedPosition: downPos

	self announcer announce: (TxSliderScrollBackward source: self).

	self world 
		startStepping: self 
		at: Time millisecondClockValue
		selector: #animateScrollingBackward: arguments: { downPos } stepTime: self scrollingDelay.
	