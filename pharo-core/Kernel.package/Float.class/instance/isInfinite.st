testing
isInfinite
	"Return true if the receiver is positive or negative infinity."

	^ self = Infinity or: [self = NegativeInfinity]
