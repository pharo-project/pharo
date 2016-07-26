factory
createEventDuringMove
	^ self eventClassMoveGesture new
		timestamp: eventHandler lastUpdateTimestamp;
		direction: direction;
		dDist: self dDist;
		deviceId: deviceId;
		position: self centerOfAllFingers;
		yourself