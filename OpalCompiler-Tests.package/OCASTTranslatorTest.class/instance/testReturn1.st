testing - simple
testReturn1
	| ir ast aCompiledMethod instance |
	ast := (OCOpalExamples>>#exampleReturn1) parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	instance := OCOpalExamples new.
	
	aCompiledMethod := ir compiledMethod.
	self assert: (aCompiledMethod valueWithReceiver: instance arguments: #()) = instance exampleReturn1.
	
	