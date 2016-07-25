tests - special cases
testExampleSimpleBlockLocalWhile
	| ast assignment vars scopes |
	ast := (OCOpalExamples>>#exampleSimpleBlockLocalWhile) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.
	
	self assert: ast scope tempVars size = 0.
	self assert: ast scope tempVector size = 1.
	self assert: (ast scope lookupVar: 'a') isRemote.
	
	scopes := (OCScopesCollector new visitNode: ast) scopes.
	
	self assert: scopes second tempVars size = 1.
	self assert: scopes second tempVector size = 0.
	self deny: (scopes second lookupVar: 'b') isRemote.
	self assert: (scopes second lookupVar: 'b') isArg.
	self deny: (scopes fourth lookupVar: 'hallo') isRemote.
	


	

	
	


