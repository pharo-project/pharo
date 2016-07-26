factory
createEventDuringMove
	^ self eventClassMoveGesture new
		timestamp: eventHandler lastUpdateTimestamp;
		deviceId: deviceId;
		position: self centerOfAllFingers;
		dDist: self dDist;
		yourself