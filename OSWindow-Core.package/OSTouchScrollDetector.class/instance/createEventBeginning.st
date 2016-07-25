factory
createEventBeginning
	directionAtBeginning := direction.
	^ self eventClassBeginGesture new
		timestamp: eventHandler lastUpdateTimestamp;
		dDist: self dDist;
		direction: direction;
		deviceId: deviceId;
		position: self centerOfAllFingers;
		yourself