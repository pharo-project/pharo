testing - blocks
testExampleMethodTempInNestedBlock
	| ir ast aCompiledMethod instance |
	ast := (OCOpalExamples>>#exampleMethodTempInNestedBlock) parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	instance := OCOpalExamples new.
	
	aCompiledMethod := ir compiledMethod.
	self assert: ((aCompiledMethod valueWithReceiver: instance arguments: #()) = instance exampleMethodTempInNestedBlock)
	