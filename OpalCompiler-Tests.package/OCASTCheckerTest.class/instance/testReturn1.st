testing - simple
testReturn1
	| ast |
	ast := (OCOpalExamples>>#exampleReturn1) parseTree.
	self nameAnalysisNoClosureIn: OCOpalExamples for: ast.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars isEmpty.