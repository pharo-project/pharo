Extension { #name : 'Time' }

{ #category : '*Deprecated14' }
Time class >> primMillisecondClock [
	"Primitive. Answer the number of milliseconds since the millisecond clock
	 was last reset or rolled over. Answer zero if the primitive fails.
	As an alternative you can use #primUTCMillisecondsClock which does not overflow."

	self deprecated: 'Use #millisecondClockValue instead' transformWith: '`@rcv primMillisecondClock' -> '`@rcv millisecondClockValue'.
	^ self millisecondClockValue
]
