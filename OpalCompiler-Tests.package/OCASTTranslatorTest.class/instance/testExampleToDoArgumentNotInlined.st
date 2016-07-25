testing - blocks - optimized
testExampleToDoArgumentNotInlined
	| ir ast aCompiledMethod instance |
	ast := (OCOpalExamples>>#exampleToDoArgumentNotInlined) parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	instance := OCOpalExamples new.
	
	aCompiledMethod := ir compiledMethod.
	self assert: (aCompiledMethod valueWithReceiver: instance arguments: #()) = instance exampleToDoArgumentNotInlined.
	
	