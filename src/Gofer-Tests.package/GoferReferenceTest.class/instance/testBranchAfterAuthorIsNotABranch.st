tests
testBranchAfterAuthorIsNotABranch
	| queryReference |
	queryReference := GoferVersionReference name: 'Seaside-Core-jf.configcleanup.3'.
	self assert: queryReference packageName = 'Seaside-Core'.
	self assert: queryReference author = 'jf.configcleanup'.
	self assert: queryReference branch isEmpty.
	self assert: queryReference versionNumber = 3.
	
	queryReference := GoferVersionReference name: 'Seaside-Core-lr.configcleanup.extraspeedup.69'.
	self assert: queryReference packageName = 'Seaside-Core'.
	self assert: queryReference author = 'lr.configcleanup.extraspeedup'.
	self assert: queryReference branch isEmpty.
	self assert: queryReference versionNumber = 69.

	queryReference := GoferVersionReference name: 'Seaside-Core-lr.configcleanup42.extraspeedup.69'.
	self assert: queryReference packageName = 'Seaside-Core'.
	self assert: queryReference author = 'lr.configcleanup42.extraspeedup'.
	self assert: queryReference branch isEmpty.
	self assert: queryReference versionNumber = 69
