tests
testRefactoring
	self 
		assert: EpPluggableFilter refactoringScopeFilter
		accepts: self refactoring
		rejects: self classAddition