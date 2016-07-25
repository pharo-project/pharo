testing - blocks - optimized
testExampleIfNotNilReturnNil
	| ir ast aCompiledMethod instance |
	ast := (OCOpalExamples>>#exampleIfNotNilReturnNil) parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	instance := OCOpalExamples new.
	
	aCompiledMethod := ir compiledMethod.
	self assert: (aCompiledMethod valueWithReceiver: instance arguments: #()) = instance exampleIfNotNilReturnNil.
	