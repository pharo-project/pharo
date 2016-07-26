gesture detection
checkDoubleTouch
	(timestamp - previousTimestamp < doubleTouchSpeed 
		and: [ (position dist: previousPosition) < maxDistanceBetweenEachTouch ])
		ifFalse: [ ^ self ].
	self sendToEventHandler: self createEvent.
	self reinitialize