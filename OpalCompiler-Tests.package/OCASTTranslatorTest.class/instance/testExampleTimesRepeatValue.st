testing - blocks - optimized
testExampleTimesRepeatValue
	| ir ast aCompiledMethod instance |
	ast := (OCOpalExamples>>#exampleTimesRepeatValue) parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	instance := OCOpalExamples new.
	
	aCompiledMethod := ir compiledMethod.
	self assert: (aCompiledMethod valueWithReceiver: instance arguments: #()) = instance exampleTimesRepeatValue.
	
	