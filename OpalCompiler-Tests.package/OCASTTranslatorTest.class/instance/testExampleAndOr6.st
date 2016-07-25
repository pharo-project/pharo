testing - blocks - optimized
testExampleAndOr6
	| ir ast aCompiledMethod instance |
	ast := (OCOpalExamples>>#exampleAndOr6) parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	instance := OCOpalExamples new.
	
	aCompiledMethod := ir compiledMethod.
	self assert: (aCompiledMethod valueWithReceiver: instance arguments: #()) = instance exampleAndOr6.
	
	