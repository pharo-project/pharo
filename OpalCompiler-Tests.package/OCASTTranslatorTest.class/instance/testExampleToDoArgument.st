testing - blocks - optimized
testExampleToDoArgument
	| ir ast aCompiledMethod instance |
	ast := (OCOpalExamples>>#exampleToDoArgument) parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	instance := OCOpalExamples new.
	
	aCompiledMethod := ir compiledMethod.
	self assert: (aCompiledMethod valueWithReceiver: instance arguments: #()) = instance exampleToDoArgument.
	
	