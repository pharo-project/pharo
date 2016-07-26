testing - variables
testNoRemoteMethodTemp
	| ast assignment vars |
	ast := (OCOpalExamples>>#noRemoteMethodTemp) parseTree.
	self nameAnalysisNoClosureIn: OCOpalExamples for: ast.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size = 2.
	
	self deny: (ast scope lookupVar: 'block1') isEscaping.
	self deny: (ast scope lookupVar: 'block2') isEscaping.
	


