gesture detection
checkDoubleTouch
	(timestamp - previousTimestamp < doubleTouchSpeed 
		and: [ (position distanceTo: previousPosition) < maxDistanceBetweenEachTouch ])
		ifFalse: [ ^ self ].
	self sendToEventHandler: self createEvent.
	self reinitialize