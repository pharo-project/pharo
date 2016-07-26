arithmetic
< comparand
	"comparand conforms to protocol DateAndTime,
	or can be converted into something that conforms."

	| other |
	other := comparand asDateAndTime.
	^ julianDayNumber = other julianDayNumberUTC
		ifTrue: [ 
			seconds = other secondsSinceMidnightUTC
				ifTrue: [ nanos < other nanoSecond ]
				ifFalse: [ seconds < other secondsSinceMidnightUTC ] ]
		ifFalse: [ julianDayNumber < other julianDayNumberUTC ]