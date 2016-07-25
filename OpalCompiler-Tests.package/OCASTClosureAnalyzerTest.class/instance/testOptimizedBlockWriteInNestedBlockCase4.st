tests - special cases
testOptimizedBlockWriteInNestedBlockCase4
	| ast assignment vars scopes |
	ast := (OCOpalExamples>>#optimizedBlockWriteInNestedBlockCase4) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.
	
	self assert: ast scope tempVars size = 0.
	self assert: ast scope tempVector size = 1.
	self assert: (ast scope lookupVar: 't1') isRemote.
	self assert: ast scope copiedVars size = 1. "Is this correct, I think that the copied vars should be empty."
	
	scopes := (OCScopesCollector new visitNode: ast) scopes.
	self assert: scopes second tempVars size = 0.
	self assert: scopes second tempVector size = 0.
	self assert: scopes second copiedVars size = 1.
	scopes second copiedVars at: '0vector0' ifAbsent: [self fail]
	

	
	


