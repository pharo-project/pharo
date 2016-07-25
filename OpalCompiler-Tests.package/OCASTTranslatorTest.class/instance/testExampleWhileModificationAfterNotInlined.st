testing - blocks - optimized
testExampleWhileModificationAfterNotInlined
	| ir ast aCompiledMethod instance |
	ast := (OCOpalExamples>>#exampleWhileModificationAfterNotInlined) parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	instance := OCOpalExamples new.
	
	aCompiledMethod := ir compiledMethod.
	self assert: (aCompiledMethod valueWithReceiver: instance arguments: #()) = instance exampleWhileModificationAfterNotInlined.
	
	