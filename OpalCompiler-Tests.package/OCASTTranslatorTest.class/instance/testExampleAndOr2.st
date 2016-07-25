testing - blocks - optimized
testExampleAndOr2
	| ir ast aCompiledMethod instance |
	ast := (OCOpalExamples>>#exampleAndOr2) parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	instance := OCOpalExamples new.
	
	aCompiledMethod := ir compiledMethod.
	self assert: (aCompiledMethod valueWithReceiver: instance arguments: #()) = instance exampleAndOr2.
	
	