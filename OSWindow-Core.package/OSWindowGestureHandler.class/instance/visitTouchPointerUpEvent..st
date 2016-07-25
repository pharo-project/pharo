visiting
visitTouchPointerUpEvent: anEvent
	self untrackFinger: anEvent.
	lastUpdateTimestamp := anEvent timestamp.
	self updateGesturesActiveFingers.
	self sendEventToGestures: anEvent.
	self visitTouchActionPointerUpEvent: anEvent