visiting
visitTouchPointerDownEvent: anEvent
	super visitTouchPointerDownEvent: anEvent.
	previousPosition := position.
	previousTimestamp := timestamp.
	position := anEvent position.
	timestamp := anEvent timestamp.
	self checkDoubleTouch.