enumerating
to: anEnd by: aDuration
	"Answer a Timespan. anEnd conforms to protocol DateAndTime or protocol Timespan"

	^ (Schedule starting: self ending: (anEnd asDateAndTime))
		schedule: (Array with: aDuration asDuration);
		yourself.
