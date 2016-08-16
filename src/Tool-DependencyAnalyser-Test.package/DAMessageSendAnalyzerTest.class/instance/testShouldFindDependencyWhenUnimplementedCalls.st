tests
testShouldFindDependencyWhenUnimplementedCalls
	self 
		assert: analyzer missingMethods size 
		equals: 3