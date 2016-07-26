initialization
initialize
	super initialize.
	waitingTime := self class waitingTime.
	maxDistanceTravelled := self class maxDistanceTravelled.
	process := Process new terminate.