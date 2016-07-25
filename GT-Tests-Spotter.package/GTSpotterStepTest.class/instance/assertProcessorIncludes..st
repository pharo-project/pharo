convenience
assertProcessorIncludes: anObject
	self assert: anObject notNil.
	self assert: currentProcessor notNil.
	self assert: currentProcessor allFilteredCandidates notEmpty.
	
	self assert: (currentProcessor allFilteredCandidates anySatisfy: [ :candidate | candidate = anObject ])