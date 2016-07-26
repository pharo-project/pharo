testing - blocks - optimized
testExampleMethodWithOptimizedBlocksA
	| ir ast aCompiledMethod instance |
	ast  := (OCOpalExamples>>#exampleMethodWithOptimizedBlocksA) parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	instance := OCOpalExamples new .
	
	aCompiledMethod := ir compiledMethod.
	self assert: (aCompiledMethod valueWithReceiver: instance arguments: #()) = instance exampleMethodWithOptimizedBlocksA.
	
	