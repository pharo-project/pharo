factory
createEventEnding
	^ self eventClassEndGesture new
		timestamp: eventHandler lastUpdateTimestamp;
		dTheta: currentAngle - lastAngle;
		deviceId: deviceId;
		position: self centerOfAllFingers;
		yourself