convenience
assertProcessorNotEmpty
	self assert: currentProcessor notNil.
	self deny: currentProcessor allFilteredCandidates isEmpty.