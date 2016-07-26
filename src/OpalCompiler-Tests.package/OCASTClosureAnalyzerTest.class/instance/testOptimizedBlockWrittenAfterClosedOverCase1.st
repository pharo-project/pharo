tests - special cases
testOptimizedBlockWrittenAfterClosedOverCase1
	| ast assignment vars scopes |
	ast := (OCOpalExamples>>#optimizedBlockWrittenAfterClosedOverCase1) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.
	
	self assert: ast scope tempVars size = 1.
	self assert: ast scope tempVector size = 0.
	
	scopes := (OCScopesCollector new visitNode: ast) scopes.
	
	self assert: scopes third  tempVars size = 0.
	self assert: scopes third  tempVector size = 1.
	
	self deny: (scopes third lookupVar: 'index') isRemote.
		"problem: as block is optimized, this var does not need to be remote"
	self assert: (scopes third tempVector at: 'temp') isRemote.

	
	


