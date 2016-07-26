testing - blocks - optimized
testExampleToDoOutsideTempNotInlined
	| ir ast aCompiledMethod instance |
	ast := (OCOpalExamples>>#exampleToDoOutsideTempNotInlined) parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	instance := OCOpalExamples new.
	
	aCompiledMethod := ir compiledMethod.
	self assert: (aCompiledMethod valueWithReceiver: instance arguments: #()) = instance exampleToDoOutsideTempNotInlined.
	
	