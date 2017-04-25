stepping
stepAt: millisecondClockValue
	"An alarm will also be fired the first time the window is opened, 
	even if stepTime was not reached. Ignore the very first step."
	hasInitialStep ifFalse: [ 
		hasInitialStep := true.
		^ self ].
	super stepAt: millisecondClockValue