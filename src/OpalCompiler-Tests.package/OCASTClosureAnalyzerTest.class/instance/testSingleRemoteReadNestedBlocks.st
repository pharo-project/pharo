tests - special cases
testSingleRemoteReadNestedBlocks
	| ast assignment vars |
	ast := (OCOpalExamples>>#singleRemoteReadNestedBlocks) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size = 0.
	self assert: ast scope tempVector size = 1.
	
	self assert: (ast scope lookupVar: 'a') isRemote.

