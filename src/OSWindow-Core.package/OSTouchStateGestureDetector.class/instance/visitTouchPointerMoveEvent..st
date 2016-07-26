visiting
visitTouchPointerMoveEvent: anEvent
	super visitTouchPointerMoveEvent: anEvent.
	self tryToGoThroughNextState.
	(modeActivated and: [ self beginConditions ])
		ifTrue: [ self sendToEventHandler: self createEventDuringMove ]