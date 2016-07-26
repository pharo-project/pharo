enumerating
to: anEnd
	"Answer a Timespan. anEnd conforms to protocol DateAndTime or protocol Timespan"

	^ Timespan starting: self ending: (anEnd asDateAndTime).
