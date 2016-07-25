visiting
visitTouchPointerDownEvent: anEvent
	super visitTouchPointerDownEvent: anEvent.
	eventHandler fingerCount = self necessaryFingers 
		ifTrue:[ vectorAtBeginnig := self vectorBetweenFirstAndLastFingers ]