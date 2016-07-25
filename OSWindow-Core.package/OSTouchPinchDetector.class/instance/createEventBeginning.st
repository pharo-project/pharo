factory
createEventBeginning
	fingersDistanceFromCenter := self fingersDistanceFromCenter.
	^ self eventClassBeginGesture new
		timestamp: eventHandler lastUpdateTimestamp;
		deviceId: deviceId;
		position: self centerOfAllFingers;
		dDist: self dDist;
		yourself