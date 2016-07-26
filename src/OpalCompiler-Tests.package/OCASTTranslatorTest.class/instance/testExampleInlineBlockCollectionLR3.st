testing - blocks
testExampleInlineBlockCollectionLR3
	| ir ast aCompiledMethod instance |
	ast := (OCOpalExamples>>#exampleInlineBlockCollectionLR3) parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	instance := OCOpalExamples new.
	
	aCompiledMethod := ir compiledMethod.
	self assert: (aCompiledMethod valueWithReceiver: instance arguments: #()) = (2 to: 12) asArray.
	