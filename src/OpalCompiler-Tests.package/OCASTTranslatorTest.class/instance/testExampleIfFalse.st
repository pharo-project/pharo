testing - simple
testExampleIfFalse
	| ir ast aCompiledMethod instance |
	ast := (OCOpalExamples>>#exampleIfFalse) parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	instance := OCOpalExamples new.
	
	aCompiledMethod := ir compiledMethod.
	self assert: (aCompiledMethod valueWithReceiver: instance arguments: #()) = instance exampleIfFalse.
	
	