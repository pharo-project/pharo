testing - blocks
testExampleWhileModificationBefore
	| ir ast aCompiledMethod instance |
	ast := (OCOpalExamples>>#exampleWhileModificationBefore) parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	instance := OCOpalExamples new.
	
	aCompiledMethod := ir compiledMethod.
	self assert: (aCompiledMethod valueWithReceiver: OCOpalExamples new arguments: #()) = instance exampleWhileModificationBefore.
	