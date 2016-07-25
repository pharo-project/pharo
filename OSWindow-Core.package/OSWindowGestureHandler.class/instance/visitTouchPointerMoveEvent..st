visiting
visitTouchPointerMoveEvent: anEvent
	| fingerTracker |
	fingerTracker := fingers at: anEvent fingerId ifAbsent: [ self trackFinger: anEvent].

	fingerTracker updateWithEvent: anEvent.
	lastUpdateTimestamp := anEvent timestamp.
	
	self sendEventToGestures: anEvent.

	self visitTouchActionPointerMoveEvent: anEvent