tests - special cases
testExampleSimpleBlockNested
	| ast assignment vars scopes |
	ast := (OCOpalExamples>>#exampleSimpleBlockNested) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.
	
	self assert: ast scope tempVars size = 2.
	self assert: ast scope tempVector size = 1.
	self deny: (ast scope lookupVar: 'a') isRemote.
	self deny: (ast scope lookupVar: 'dict') isRemote.
	self assert: (ast scope lookupVar: 'match') isRemote.
	
	scopes := (OCScopesCollector new visitNode: ast) scopes.
	
	self assert: scopes second tempVars size = 2.
	self assert: scopes second tempVector size = 0.
	self deny: (scopes second lookupVar: 'each') isRemote.
	self deny: (scopes second lookupVar: 'index') isRemote.
	
	self assert: scopes second copiedVars size = 3


	

	
	


