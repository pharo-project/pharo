accessing
tempAt: zeroBasedIndex
	temps := temps max: zeroBasedIndex + 1.
	^ self -> zeroBasedIndex