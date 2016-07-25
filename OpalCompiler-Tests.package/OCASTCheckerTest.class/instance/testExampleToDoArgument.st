testing - blocks - optimized
testExampleToDoArgument
	| ast |
	ast := (OCOpalExamples>>#exampleToDoArgument) parseTree.
	self nameAnalysisNoClosureIn: OCOpalExamples for: ast.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size = 0.