testing - blocks
testExampleSimpleBlockArgument4
	| ir ast aCompiledMethod instance |
	ast := (OCOpalExamples>>#exampleSimpleBlockArgument4) parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	instance := OCOpalExamples new.
	
	aCompiledMethod := ir compiledMethod.
	self assert: (aCompiledMethod valueWithReceiver: instance arguments: #()) = instance exampleSimpleBlockArgument4 .
	