truncation
rounded
	"Answer a date and time to the nearest whole second"

	^ self species basicNew
		ticks:
			{ julianDayNumber.
			nanos *2 >= NanosInSecond
				ifTrue: [seconds + 1]
				ifFalse: [seconds].
			0 }
		offset: offset