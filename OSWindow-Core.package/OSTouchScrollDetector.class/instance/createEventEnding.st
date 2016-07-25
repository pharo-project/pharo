factory
createEventEnding
	directionAtBeginning := 0@0.
	^ self eventClassEndGesture new
		timestamp: eventHandler lastUpdateTimestamp;
		dDist: self dDist;
		direction: direction;
		deviceId: deviceId;
		position: self centerOfAllFingers;
		yourself