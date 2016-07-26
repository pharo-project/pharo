testing - blocks - optimized
testExampleSimpleBlockLocalIf
	| ir ast aCompiledMethod instance |
	ast := (OCOpalExamples>>#exampleSimpleBlockLocalIf) parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	instance := OCOpalExamples new.
	
	aCompiledMethod := ir compiledMethod.
	self assert: (aCompiledMethod valueWithReceiver: instance arguments: #()) = instance exampleSimpleBlockLocalIf.
	
	