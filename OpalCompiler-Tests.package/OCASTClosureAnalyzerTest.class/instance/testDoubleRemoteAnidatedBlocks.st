tests - special cases
testDoubleRemoteAnidatedBlocks
	| ast assignment vars scopes |
	ast := (OCOpalExamples>>#doubleRemoteAnidatedBlocks) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size = 0.
	self assert: ast scope tempVector size = 2.
	
	scopes := (OCScopesCollector new visitNode: ast) scopes.
	
	self assert: (ast scope lookupVar: 'last') isRemote.
	self assert: (ast scope lookupVar: 'val') isRemote.
	self assert: (ast scope lookupVar: 'val') vectorName = '0vector0'.
	self deny: (scopes second  lookupVar: 'i') isRemote. 
	

	
	


