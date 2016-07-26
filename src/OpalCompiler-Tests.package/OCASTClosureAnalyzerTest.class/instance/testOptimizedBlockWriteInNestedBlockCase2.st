tests - special cases
testOptimizedBlockWriteInNestedBlockCase2
	| ast assignment vars scopes |
	ast := (OCOpalExamples>>#optimizedBlockWriteInNestedBlockCase2) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.
	
	self assert: ast scope tempVars size = 1.
	self assert: ast scope tempVector size = 0.
	self deny: (ast scope lookupVar: 't1') isRemote.
	self assert: ast scope copiedVars size = 0.
	


	

	
	


