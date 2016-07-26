testing - blocks
testExampleBlockExternalArg
	| ir ast aCompiledMethod instance |
	ast := (OCOpalExamples>>#exampleBlockExternalArg) parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	instance := OCOpalExamples new.
	
	aCompiledMethod := ir compiledMethod.
	self assert: (aCompiledMethod valueWithReceiver: instance arguments: #()) = instance exampleBlockExternalArg.
	