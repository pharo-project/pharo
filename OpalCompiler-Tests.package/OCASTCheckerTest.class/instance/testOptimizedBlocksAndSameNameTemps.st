testing - variables
testOptimizedBlocksAndSameNameTemps
	| ast assignment vars scopes |
	ast := (OCOpalExamples>>#optimizedBlocksAndSameNameTemps) parseTree.
	self nameAnalysisNoClosureIn: OCOpalExamples for: ast.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size = 2.
	
	self deny: (ast scope lookupVar: 's') isRemote.
	self deny: (ast scope lookupVar: 'c') isRemote.
	
	scopes := (OCScopesCollector new visitNode: ast) scopes.
	
	self deny: (scopes second lookupVar: 'a') isRemote.
	self deny: (scopes fourth lookupVar: 'i') isRemote.


