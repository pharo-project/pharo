testing - blocks
testExampleBlockMethodArgument
	| ir ast aCompiledMethod instance |
	ast := (OCOpalExamples>>#exampleBlockMethodArgument:) parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	instance := OCOpalExamples new.
	
	aCompiledMethod := ir compiledMethod.
	self assert: (aCompiledMethod valueWithReceiver: instance arguments: #(2)) = 4.
	