visiting
visitTouchPointerMoveEvent: anEvent
	super visitTouchPointerMoveEvent: anEvent.
	eventHandler fingerCount = self necessaryFingers 
		ifTrue:[ self updateCurrentAngle ]