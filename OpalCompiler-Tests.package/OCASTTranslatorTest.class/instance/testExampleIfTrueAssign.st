testing - blocks - optimized
testExampleIfTrueAssign
	| ir ast aCompiledMethod instance |
	ast := (OCOpalExamples>>#exampleIfTrueAssign) parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	instance := OCOpalExamples new.
	
	aCompiledMethod := ir compiledMethod.
	self assert: (aCompiledMethod valueWithReceiver: instance arguments: #()) = 1.
	