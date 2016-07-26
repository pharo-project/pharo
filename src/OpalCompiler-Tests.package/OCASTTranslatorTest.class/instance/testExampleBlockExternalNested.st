testing - blocks
testExampleBlockExternalNested
	| ir ast aCompiledMethod instance |
	ast := (OCOpalExamples>>#exampleBlockExternalNested) parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	instance := OCOpalExamples new.
	
	aCompiledMethod := ir compiledMethod.
	self assert: (aCompiledMethod valueWithReceiver: instance arguments: #()) = instance exampleBlockExternalNested.
	