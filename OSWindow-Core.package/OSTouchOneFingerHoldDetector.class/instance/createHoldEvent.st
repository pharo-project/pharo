creation
createHoldEvent
	^ OSTouchOneFingerHoldEvent new 
		position: self centerOfAllFingers;
		timestamp: eventHandler lastUpdateTimestamp ;
		deviceId: deviceId;
		yourself