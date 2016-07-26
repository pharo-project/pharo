enumerating
to: anEnd by: aDuration do: aBlock
	"Answer a Timespan. anEnd conforms to protocol DateAndTime or protocol Timespan"

	^ (self to: anEnd by: aDuration) scheduleDo: aBlock
