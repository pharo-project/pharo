testing - blocks
testExampleBlockExternal2
	| ir ast aCompiledMethod instance |
	ast := (OCOpalExamples>>#exampleBlockExternal2) parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	instance := OCOpalExamples new.
	
	aCompiledMethod := ir compiledMethod.
	self assert: (aCompiledMethod valueWithReceiver: instance arguments: #()) = instance exampleBlockExternal2.
	