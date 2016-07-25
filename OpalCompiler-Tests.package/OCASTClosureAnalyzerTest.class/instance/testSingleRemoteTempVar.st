tests - special cases
testSingleRemoteTempVar
	| ast |
	ast := (OCOpalExamples>>#singleRemoteTempVar) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size = 2.
	self assert: ast scope tempVector size = 1.
	
	self assert: (ast scope lookupVar: 'index') isRemote.
	self assert: (ast scope lookupVar: 'index') definingScope = ast scope.
	self deny: (ast scope lookupVar: 'block') isRemote.
	self deny: (ast scope lookupVar: 'collection') isRemote.


