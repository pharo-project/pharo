testing - blocks - optimized
testExampleInlineBlockCollectionLR3
	| ast |
	ast := (OCOpalExamples>>#exampleInlineBlockCollectionLR3) parseTree.
	self nameAnalysisNoClosureIn: OCOpalExamples for: ast.
	self assert: ast scope isMethodScope.
	self assert: ast scope tempVars size = 1. "index is a temp of the outer method due to optimized block"