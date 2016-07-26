testing - variables
testsingleRemoteTempVarWrittenAfterClosedOver
	| ast |
	ast := (OCOpalExamples>>#singleRemoteTempVarWrittenAfterClosedOver) parseTree.
	self nameAnalysisNoClosureIn: OCOpalExamples for: ast.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size = 2.
	
	self assert: (ast scope lookupVar: 'index') isWrite.
	self assert: (ast scope lookupVar: 'index') definingScope = ast scope.
	self deny: (ast scope lookupVar: 'block') isEscaping.
	self assert: (ast scope lookupVar: 'block') isTemp.

