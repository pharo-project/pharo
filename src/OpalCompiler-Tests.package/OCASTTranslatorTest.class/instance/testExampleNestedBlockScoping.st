testing - blocks
testExampleNestedBlockScoping
	| ir ast aCompiledMethod instance |
	ast := (OCOpalExamples>>#exampleNestedBlockScoping) parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	instance := OCOpalExamples new.
	
	aCompiledMethod := ir compiledMethod.
	self assert: (aCompiledMethod valueWithReceiver: instance arguments: #()) = instance exampleNestedBlockScoping.
	