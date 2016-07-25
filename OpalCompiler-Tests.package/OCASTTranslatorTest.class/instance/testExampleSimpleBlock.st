testing - blocks
testExampleSimpleBlock
	| ir ast aCompiledMethod instance |
	ast := (OCOpalExamples>>#exampleSimpleBlock) parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	instance := OCOpalExamples new.
	
	aCompiledMethod := ir compiledMethod.
	self assert: (aCompiledMethod valueWithReceiver: instance arguments: #()) value = instance exampleSimpleBlock value.
	