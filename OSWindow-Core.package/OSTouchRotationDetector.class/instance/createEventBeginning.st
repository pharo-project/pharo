factory
createEventBeginning
	vectorAtBeginnig := self vectorBetweenFirstAndLastFingers.
	currentAngle := vectorAtBeginnig angleWith: self vectorBetweenFirstAndLastFingers.
	self updateCurrentAngle .
	
	^ self eventClassBeginGesture new
		timestamp: eventHandler lastUpdateTimestamp;
		dTheta: 0;
		deviceId: deviceId;
		position: self centerOfAllFingers;
		yourself