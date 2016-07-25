tests - special cases
testNoRemoteReadNestedBlocks
	| ast assignment vars scopes |
	ast := (OCOpalExamples>>#noRemoteReadNestedBlocks) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size = 0.
	self assert: ast scope tempVector size = 0.
	
	scopes := (OCScopesCollector new visitNode: ast) scopes.
	self assert: scopes second tempVars size = 1.
	self assert: scopes second tempVector size = 0.
	self deny: (scopes second lookupVar: 'a') isRemote.

	

	
	


