testing - blocks - optimized
testExampleOptimizedBlockWrittenAfterClosedOverCase1
	| ir ast aCompiledMethod instance |
	ast  := (OCOpalExamples>>#optimizedBlockWrittenAfterClosedOverCase1) parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	instance := OCOpalExamples new .
	
	aCompiledMethod := ir compiledMethod.
	self assert: (aCompiledMethod valueWithReceiver: instance arguments: #()) = instance optimizedBlockWrittenAfterClosedOverCase1.
	
	