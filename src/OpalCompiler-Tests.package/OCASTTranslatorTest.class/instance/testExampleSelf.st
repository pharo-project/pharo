testing - variables
testExampleSelf
	| ir ast aCompiledMethod instance |
	ast := (OCOpalExamples>>#exampleSelf) parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	instance := OCOpalExamples new.
	
	aCompiledMethod := ir compiledMethod.
	self assert: (aCompiledMethod valueWithReceiver: instance arguments: #()) = instance exampleSelf.
	
	