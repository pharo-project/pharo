testing - blocks
testExampleBlockArgument
	| ir ast aCompiledMethod instance |
	ast := (OCOpalExamples>>#exampleBlockArgument) parseTree.
	ir := ast doSemanticAnalysis ir.
	instance := OCOpalExamples new.
	
	aCompiledMethod := ir compiledMethod.
	self assert: (aCompiledMethod valueWithReceiver: instance arguments: #()) = instance exampleBlockArgument.
	