testing - blocks
testExampleBlockNested
	| ir ast aCompiledMethod instance |
	ast := (OCOpalExamples>>#exampleBlockNested) parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	instance := OCOpalExamples new.
	
	aCompiledMethod := ir compiledMethod.
	self assert: (aCompiledMethod valueWithReceiver: instance arguments: #()) = instance exampleBlockNested.
	