testing - primitives
testExamplePrimitiveErrorCode
	| method ast ir newMethod |
	ast := (OCOpalExamples>>#examplePrimitiveErrorCode) parseTree.
	self nameAnalysisNoClosureIn: OCOpalExamples for: ast.

	self assert: (ast scope lookupVar: 'code') isTemp.

	