tests - special cases
testOptimizedBlockWrittenAfterClosedOverCase2
	| ast assignment vars scopes |
	ast := (OCOpalExamples>>#optimizedBlockWrittenAfterClosedOverCase2) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.
	
	self assert: ast scope tempVars size = 1.
	self assert: ast scope tempVector size =0.
	self deny: (ast scope lookupVar: 'index') isRemote.
	
	scopes := (OCScopesCollector new visitNode: ast) scopes.
	self assert: (scopes third tempVector at: 'temp') isRemote.

	
	


