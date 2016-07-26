testing - blocks - optimized
testExampleAndOr5
	| ir ast aCompiledMethod instance |
	ast := (OCOpalExamples>>#exampleAndOr5) parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	instance := OCOpalExamples new.
	
	aCompiledMethod := ir compiledMethod.
	self assert: (aCompiledMethod valueWithReceiver: instance arguments: #()) = instance exampleAndOr5.
	
	