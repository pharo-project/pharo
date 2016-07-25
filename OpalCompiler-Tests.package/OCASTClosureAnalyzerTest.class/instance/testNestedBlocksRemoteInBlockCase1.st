tests - special cases
testNestedBlocksRemoteInBlockCase1
	| ast scopes |
	ast := (OCOpalExamples>>#nestedBlocksRemoteInBlockCase1) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.
	
	self assert: ast scope tempVars size = 1.
	self assert: ast scope tempVector size = 0.
	self deny: (ast scope lookupVar: 'block') isRemote.
	
	scopes := (OCScopesCollector new visitNode: ast) scopes.
	
	self assert: scopes second tempVars size = 0.
	self assert: scopes second tempVector size = 2.
	self assert: (scopes second tempVector at: 'a') isRemote.
	self assert: (scopes second tempVector at: 'b') isRemote.


	

	
	


