testing - variables
testsingleRemoteTempVarWhileWithTempNotInlined
	| ast |
	ast := (OCOpalExamples>>#exampleWhileWithTempNotInlined) parseTree.
	self nameAnalysisNoClosureIn: OCOpalExamples for: ast.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size = 2.
	
	self assert: (ast scope lookupVar: 'index') isEscaping.
	self assert: (ast scope lookupVar: 'index') definingScope = ast scope.
	self deny: (ast scope lookupVar: 'block') isEscaping.
	self assert: (ast scope lookupVar: 'block') isTemp.


