testing - variables
testSingleRemoteDifferentBlocksSameArgumentName
	| ast assignment vars |
	ast := (OCOpalExamples>>#singleRemoteDifferentBlocksSameArgumentName) parseTree.
	self nameAnalysisNoClosureIn: OCOpalExamples for: ast.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size = 3.
	
	self deny: (ast scope lookupVar: 'b') isEscaping.
	self deny: (ast scope lookupVar: 'c') isEscaping.
	self assert: (ast scope lookupVar: 'z') isEscaping.


