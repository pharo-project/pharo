convenience
assertProcessorIncludesAll: aCollection
	self assert: aCollection notEmpty.
	self assert: currentProcessor notNil.
	self assert: currentProcessor allFilteredCandidates notEmpty.
	
	aCollection do: [ :each | 
		self assert: (currentProcessor allFilteredCandidates anySatisfy: [ :candidate | 
			candidate = each ]) ]