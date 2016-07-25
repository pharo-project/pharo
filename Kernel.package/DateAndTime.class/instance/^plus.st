arithmetic
+ operand
	"operand conforms to protocol Duration"

	| durationTicks |
	durationTicks := operand asDuration ticks.
	^ self class basicNew
		setJdn: julianDayNumber + durationTicks first
			seconds: seconds + durationTicks second
			nano: nanos + durationTicks third
			offset: self offset;
		yourself