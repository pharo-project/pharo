general inquiries
microsecondsToRun: timedBlock 
	"Answer the number of milliseconds timedBlock takes to return its value."

	| initialMicroseconds |
	initialMicroseconds := self microsecondClockValue.
	timedBlock value.
	^self microsecondClockValue - initialMicroseconds