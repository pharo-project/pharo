computing
missingDependencies
	^ (self missingMethodsWithPotentialMatch
		select: [ :each | each size = 1 ]) flattened asSet