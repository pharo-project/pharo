testing - blocks - optimized
testExampleToDoInsideTemp
	| ir ast aCompiledMethod instance |
	ast := (OCOpalExamples>>#exampleToDoInsideTemp) parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	instance := OCOpalExamples new.
	
	aCompiledMethod := ir compiledMethod.
	self assert: (aCompiledMethod valueWithReceiver: instance arguments: #()) = instance exampleToDoInsideTemp.
	
	