stepping
adjustWakeupTimesIfNecessary
	"Fix the wakeup times in my step list if necessary. This is needed after a snapshot, after a long pause (say because some other view had control or because the user was selecting from an MVC-style menu) or when the millisecond clock wraps around (a very rare occurence with a 32-bit clock!)."

	| now |
	now := Time millisecondClockValue.
	((now < lastStepTime) or: [(now - lastStepTime) > 5000])
		 ifTrue: [self adjustWakeupTimes: now].  "clock slipped"
