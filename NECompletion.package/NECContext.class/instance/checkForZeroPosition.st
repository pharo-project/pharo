private-compute-index
checkForZeroPosition
	^ (position = 0
			or: [ranges isEmpty])
		and: [self insertEmptyRangeAt: 1 start: 0 end: 1.
			true]