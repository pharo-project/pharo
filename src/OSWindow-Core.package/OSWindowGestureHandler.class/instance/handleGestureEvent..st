events
handleGestureEvent: anEvent
	self handleEvent: anEvent.
	
	"Notifying gestures"
	self sendEventToGestures: anEvent