tests - special cases
testOptimizedBlockWriteInBlock
	| ast assignment vars scopes |
	ast := (OCOpalExamples>>#optimizedBlockWriteInBlock) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.
	
	self assert: ast scope tempVars size = 1.
	self assert: ast scope tempVector size = 0.
	self deny: (ast scope lookupVar: 't1') isRemote.
	


	

	
	


