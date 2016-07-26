factory
createEventDuringMove
	^ self eventClassMoveGesture new
		timestamp: eventHandler lastUpdateTimestamp;
		dDist: self dDist;
		direction: direction;
		deviceId: deviceId;
		position: self centerOfAllFingers;
		yourself