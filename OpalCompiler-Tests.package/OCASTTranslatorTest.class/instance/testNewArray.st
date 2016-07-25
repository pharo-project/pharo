testing - simple
testNewArray
	| ir ast aCompiledMethod instance |
	ast := (OCOpalExamples>>#exampleNewArray) parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	instance := OCOpalExamples new.
	
	aCompiledMethod := ir compiledMethod.
	self assert: (aCompiledMethod valueWithReceiver: instance arguments: #()) = instance exampleNewArray.
	
	