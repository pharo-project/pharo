accessing
tests
	^(OrderedCollection new: self runCount)
		addAll: passed;
		addAll: failures;
		addAll: errors;
		addAll: skipped;
		yourself