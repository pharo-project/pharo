visiting
visitTouchPointerUpEvent: anEvent
	super visitTouchPointerUpEvent: anEvent.
	self tryToGoThroughNextState