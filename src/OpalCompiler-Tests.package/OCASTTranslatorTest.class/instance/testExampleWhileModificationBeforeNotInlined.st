testing - blocks - optimized
testExampleWhileModificationBeforeNotInlined
	| ir ast aCompiledMethod instance |
	ast := (OCOpalExamples>>#exampleWhileModificationBeforeNotInlined) parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	instance := OCOpalExamples new.
	
	aCompiledMethod := ir compiledMethod.
	self assert: (aCompiledMethod valueWithReceiver: instance arguments: #()) = instance exampleWhileModificationBeforeNotInlined.
	
	