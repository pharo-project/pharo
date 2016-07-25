testing - blocks - optimized
testExampleIfTrue
	| ir ast aCompiledMethod instance |
	ast := (OCOpalExamples>>#exampleIfTrue) parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	instance := OCOpalExamples new.
	aCompiledMethod := ir compiledMethod.
	self assert: (aCompiledMethod valueWithReceiver: instance arguments: #()) = 'result'.
	