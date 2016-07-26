testing - variables
testDoubleRemoteAnidatedBlocks
	| ast assignment vars scopes |
	ast := (OCOpalExamples>>#doubleRemoteAnidatedBlocks) parseTree.
	self nameAnalysisNoClosureIn: OCOpalExamples for: ast.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size = 2.
	
	self assert:  (ast scope lookupVar: 'last') isEscaping.
	self assert: (ast scope lookupVar: 'val') isEscaping.
	
	scopes := (OCScopesCollector new visitNode: ast) scopes.
	self assert: scopes size = 4.
	
	self assert: (scopes second lookupVar: 'i') isEscaping. "This is due to the inlined block."

	
	self assert: scopes third tempVars size = 1.
	self deny: (scopes third lookupVar: 'continue') isEscaping. "It is not escaping since is being accessed in an optimized block."


