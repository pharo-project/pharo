testing - blocks - optimized
testExampleAndOr3
	| ir ast aCompiledMethod instance |
	ast := (OCOpalExamples>>#exampleAndOr3) parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	instance := OCOpalExamples new.
	
	aCompiledMethod := ir compiledMethod.
	self assert: (aCompiledMethod valueWithReceiver: instance arguments: #()) = instance exampleAndOr3.
	
	