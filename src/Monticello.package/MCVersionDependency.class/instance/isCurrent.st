testing
isCurrent
	^ package hasWorkingCopy
		and: [self isFulfilled
			and: [package workingCopy modified not]]