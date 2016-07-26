testing - variables
testNoRemoteBlockReturn
	| ast assignment vars |
	ast := (OCOpalExamples>>#noRemoteBlockReturn) parseTree.
	self nameAnalysisNoClosureIn: OCOpalExamples for: ast.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size = 0.
	


