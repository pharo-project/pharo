tests - special cases
testSingleRemoteDifferentBlocksSameArgumentName
	| ast assignment vars |
	ast := (OCOpalExamples>>#singleRemoteDifferentBlocksSameArgumentName) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size = 2.
	self assert: ast scope tempVector size = 1.
	
	self deny: (ast scope lookupVar: 'b') isRemote.
	self deny: (ast scope lookupVar: 'c') isRemote.
	self assert: (ast scope lookupVar: 'z') isRemote.
