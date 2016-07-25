testing - blocks
testExampleBlockInternal
	| ir ast aCompiledMethod instance |
	ast := (OCOpalExamples>>#exampleBlockInternal) parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	instance := OCOpalExamples new.
	
	aCompiledMethod := ir compiledMethod.
	self assert: (aCompiledMethod valueWithReceiver: instance arguments: #()) = instance exampleBlockInternal.
	