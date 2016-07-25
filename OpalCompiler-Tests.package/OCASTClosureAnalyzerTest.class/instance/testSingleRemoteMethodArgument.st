tests - special cases
testSingleRemoteMethodArgument
	| ast assignment vars |
	ast := (OCOpalExamples>>#singleRemoteMethodArgument) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size = 1.
	self assert: ast scope tempVector size = 1.
	
	self deny: (ast scope lookupVar: 'block') isRemote.
	self assert: (ast scope lookupVar: 'temp') isRemote.

