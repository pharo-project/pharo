testing - variables
testExampleSuper
	| ir ast aCompiledMethod instance |
	ast := (OCOpalExamples>>#exampleSuper) parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	instance := OCOpalExamples new.
	
	aCompiledMethod := ir compiledMethod.
	self assert: (aCompiledMethod valueWithReceiver: instance arguments: #()) = instance exampleSuper.
	
	