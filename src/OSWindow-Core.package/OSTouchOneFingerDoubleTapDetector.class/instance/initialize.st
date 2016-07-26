initialization
initialize
	super initialize.
	doubleTouchSpeed := self class doubleTouchSpeed.
	maxDistanceBetweenEachTouch := self class maxDistance.
	self reinitialize.