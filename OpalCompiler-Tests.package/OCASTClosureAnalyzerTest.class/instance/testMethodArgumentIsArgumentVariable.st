testing - variables
testMethodArgumentIsArgumentVariable
	| ast |
	ast := (OCOpalExamples>>#exampleWithArgument:) parseTree.
	ast doSemanticAnalysis .
	self assert: ast arguments notEmpty.
	self assert: ast arguments first isArgument
	