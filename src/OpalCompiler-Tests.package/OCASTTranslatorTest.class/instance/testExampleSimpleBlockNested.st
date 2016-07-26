testing - blocks
testExampleSimpleBlockNested
	| ir ast aCompiledMethod instance |
	ast := (OCOpalExamples>>#exampleSimpleBlockNested) parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	instance := OCOpalExamples new.
	
	aCompiledMethod := ir compiledMethod.
	self assert: (aCompiledMethod valueWithReceiver: instance arguments: #()) = instance exampleSimpleBlockNested.
	