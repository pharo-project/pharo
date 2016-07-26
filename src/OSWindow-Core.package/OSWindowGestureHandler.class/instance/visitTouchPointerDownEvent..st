visiting
visitTouchPointerDownEvent: anEvent
	self trackFinger: anEvent.
	
	"Updating informations"
	fingerCount := fingerCount + 1 .
	lastUpdateTimestamp := anEvent timestamp.
	
	self updateGesturesActiveFingers.
	
	self sendEventToGestures: anEvent.
	
	self visitTouchActionPointerDownEvent: anEvent