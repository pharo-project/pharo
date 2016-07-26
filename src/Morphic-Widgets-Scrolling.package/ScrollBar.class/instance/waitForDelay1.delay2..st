scroll timing
waitForDelay1: delay1 delay2: delay2 
	"Return true if an appropriate delay has passed since the last scroll operation.
	The delay decreases exponentially from delay1 to delay2."

	| now scrollDelay |
	timeOfLastScroll ifNil: [self resetTimer].	"Only needed for old instances"
	now := Time millisecondClockValue.
	(scrollDelay := currentScrollDelay) ifNil: [scrollDelay := delay1	"initial delay"].
	currentScrollDelay := scrollDelay * 9 // 10 max: delay2.	"decrease the delay"
	timeOfLastScroll := now.
	^true