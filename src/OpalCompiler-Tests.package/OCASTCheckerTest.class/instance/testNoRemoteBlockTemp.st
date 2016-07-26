testing - variables
testNoRemoteBlockTemp
	| ast assignment vars |
	ast := (OCOpalExamples>>#noRemoteBlockTemp) parseTree.
	self nameAnalysisNoClosureIn: OCOpalExamples for: ast.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size = 3.
	
	self deny: (ast scope lookupVar: 'block') isEscaping.
	self deny: (ast scope lookupVar: 'block1') isEscaping.
	self deny: (ast scope lookupVar: 'block2') isEscaping.


