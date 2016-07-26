testing - simple
testExampleIfNotNilReturnNil
	| ast |
	ast := (OCOpalExamples>>#exampleIfNotNilReturnNil) parseTree.
	self nameAnalysisNoClosureIn: OCOpalExamples for: ast.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size = 0.