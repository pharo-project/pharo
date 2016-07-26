testing - blocks - optimized
testExampleIfTrueIfFalse
	| ir ast aCompiledMethod instance |
	ast := (OCOpalExamples>>#exampleIfTrueIfFalse) parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	instance := OCOpalExamples new.
	aCompiledMethod := ir compiledMethod.
	self assert: (aCompiledMethod valueWithReceiver: instance arguments: #()) = 'result'.
	