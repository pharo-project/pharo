tests
testShouldGetPotentialMatchForUnimplementedCalls
	self 
		assert: (analyzer missingMethodsWithPotentialMatch values collect: #size)
		equals: #(0 2).
	self 
		assert: (analyzer missingMethodsWithPotentialMatchAfterManuallyResolvedDependenciesAddition values collect: #size)
		equals: #(0 2)