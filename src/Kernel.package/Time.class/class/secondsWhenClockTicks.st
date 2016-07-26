clock
secondsWhenClockTicks

	"waits for the moment when a new second begins"

	| lastSecond |

	lastSecond := self primUTCSecondsClock.
	[ lastSecond = self primUTCSecondsClock ] whileTrue: [ (Delay forMilliseconds: 1) wait ].
 
	^ lastSecond + 1 