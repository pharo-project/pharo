testing - blocks
testExampleSimpleBlockArgument3
	| ir ast aCompiledMethod instance |
	ast := (OCOpalExamples>>#exampleSimpleBlockArgument3) parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	instance := OCOpalExamples new.
	
	aCompiledMethod := ir compiledMethod.
	self assert: (aCompiledMethod valueWithReceiver: instance arguments: #()) = instance exampleSimpleBlockArgument3.
	