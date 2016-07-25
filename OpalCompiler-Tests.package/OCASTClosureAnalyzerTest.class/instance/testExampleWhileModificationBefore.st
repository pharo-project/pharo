tests - blocks
testExampleWhileModificationBefore
	| ast assignment vars blockScope blockScope2 |
	ast := (OCOpalExamples>>#exampleWhileModificationBefore) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size = 0.
	self assert: ast scope tempVector size = 1.
	self assert: ast scope copiedVars size = 1.
	
	self assert: (ast scope lookupVar: 'index') isRemote. 
	

	


