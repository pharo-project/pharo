testing - blocks - optimized
testExampleTimesRepeatEffect
	| ir ast aCompiledMethod instance |
	ast := (OCOpalExamples>>#exampleTimesRepeatEffect) parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	instance := OCOpalExamples new.
	
	aCompiledMethod := ir compiledMethod.
	self assert: (aCompiledMethod valueWithReceiver: instance arguments: #()) = instance exampleTimesRepeatEffect.
	
	