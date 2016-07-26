tests - special cases
testsingleRemoteTempVarWhileWithTempNotInlined
	| ast |
	ast := (OCOpalExamples>>#exampleWhileWithTempNotInlined) parseTree.
	ast doSemanticAnalysis.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size = 1.
	self assert: ast scope tempVector size = 1.
	
	self assert: (ast scope lookupVar: 'index') isRemote.
	self assert: (ast scope lookupVar: 'index') definingScope = ast scope.
	self deny: (ast scope lookupVar: 'block') isRemote.



