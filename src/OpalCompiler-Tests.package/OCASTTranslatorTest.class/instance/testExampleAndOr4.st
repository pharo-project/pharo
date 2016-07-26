testing - blocks - optimized
testExampleAndOr4
	| ir ast aCompiledMethod instance |
	ast := (OCOpalExamples>>#exampleAndOr4) parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	instance := OCOpalExamples new.
	
	aCompiledMethod := ir compiledMethod.
	self assert: (aCompiledMethod valueWithReceiver: instance arguments: #()) = instance exampleAndOr4.
	
	