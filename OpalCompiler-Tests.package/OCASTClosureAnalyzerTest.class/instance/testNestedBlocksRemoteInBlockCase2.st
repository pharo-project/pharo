tests - special cases
testNestedBlocksRemoteInBlockCase2
	| ast assignment vars scopes |
	ast := (OCOpalExamples>>#nestedBlocksRemoteInBlockCase2) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.
	
	self assert: ast scope tempVars size = 1.
	self assert: ast scope tempVector size = 0.
	self deny: (ast scope lookupVar: 'block') isRemote.
	
	scopes := (OCScopesCollector new visitNode: ast) scopes.
	
	self assert: scopes second tempVars size = 0.
	self assert: scopes second tempVector size = 0.
	
	self assert: scopes third tempVars size = 0.
	self assert: scopes third tempVector size = 1.
	self assert: (scopes third tempVector at: 'a') isRemote.
	
	self assert: scopes fourth tempVars size = 0.
	self assert: scopes fourth tempVector size = 0.
	
	self assert: scopes fifth tempVars size = 0.
	self assert: scopes fifth tempVector size = 1.
	self assert: (scopes fifth tempVector at: 'b') isRemote.


	

	
	


