factory
createEventEnding
	| endClass |
	endClass := self areFingersCloseToABorder ifTrue: [ self eventClassEndGesture ] ifFalse: [ self eventClassAbortGesture ].
	^ endClass new
		timestamp: eventHandler lastUpdateTimestamp;
		direction: direction;
		deviceId: deviceId;
		dDist: self dDist;
		position: self centerOfAllFingers;
		yourself