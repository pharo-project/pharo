tests - removing
testRemove

	self should: [self nonEmptyDict remove: nil] raise: Error.
	self should: [self nonEmptyDict remove: nil ifAbsent: ['What ever here']] raise: Error.