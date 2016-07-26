testing - variables
testSingleRemoteMethodArgument
	| ast assignment vars |
	ast := (OCOpalExamples>>#singleRemoteMethodArgument) parseTree.
	self nameAnalysisNoClosureIn: OCOpalExamples for: ast.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size = 2.
	
	self deny: (ast scope lookupVar: 'block') isEscaping.
	self assert: (ast scope lookupVar: 'temp') isEscaping.
	self assert: (ast scope lookupVar: 'temp') isWrite.


