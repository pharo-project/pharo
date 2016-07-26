arithmetic
- operand
	"operand conforms to protocol DateAndTime or protocol Duration"

	^ (operand respondsTo: #asDateAndTime)
		ifTrue: [ 
			| other |
			other := operand asDateAndTime.
			Duration
				seconds: (SecondsInDay * (julianDayNumber - other julianDayNumberUTC))
							+ (seconds - other secondsSinceMidnightUTC)
				nanoSeconds: nanos - other nanoSecond ]
		ifFalse: [ self + operand negated ]