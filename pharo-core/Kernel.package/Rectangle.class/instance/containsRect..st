testing
containsRect: aRect
	"Answer whether aRect is within the receiver (OK to coincide)."

	^ aRect origin >= origin and: [aRect corner <= corner]
