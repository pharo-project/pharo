factory
createEventEnding
	^ self eventClassEndGesture new
		timestamp: eventHandler lastUpdateTimestamp;
		deviceId: deviceId;
		position: self centerOfAllFingers;
		dDist: self dDist;
		yourself