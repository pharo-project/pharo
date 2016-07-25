testing - blocks - optimized
testExampleAndOr
	| ir ast aCompiledMethod instance |
	ast := (OCOpalExamples>>#exampleAndOr) parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	instance := OCOpalExamples new.
	
	aCompiledMethod := ir compiledMethod.
	self assert: (aCompiledMethod valueWithReceiver: instance arguments: #()) = instance exampleAndOr.
	
	