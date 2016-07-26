testing - simple
testEmptyMethod
	| ast ir aCompiledMethod instance |
	ast := (OCOpalExamples>>#exampleEmptyMethod) parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	instance := OCOpalExamples new .
	
	aCompiledMethod := ir compiledMethod.
	self assert: (aCompiledMethod valueWithReceiver: instance arguments: #()) = instance exampleEmptyMethod.
	
	