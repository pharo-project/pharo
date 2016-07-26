visiting
visitTouchPointerDownEvent: anEvent
	super visitTouchPointerDownEvent: anEvent.
	self tryToGoThroughNextState