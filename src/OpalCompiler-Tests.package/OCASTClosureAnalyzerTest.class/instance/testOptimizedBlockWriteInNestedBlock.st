tests - special cases
testOptimizedBlockWriteInNestedBlock
	| ast assignment vars scopes |
	ast := (OCOpalExamples>>#optimizedBlockWriteInNestedBlock) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.
	
	self assert: ast scope tempVars size = 0.
	self assert: ast scope tempVector size = 1.
	self assert: (ast scope lookupVar: 't1') isRemote.
	


	

	
	


