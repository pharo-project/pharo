testing - simple
testOnePlusTwo
	| ast ir aCompiledMethod instance |
	ast  := (OCOpalExamples>>#exampleReturn1plus2) parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	instance := OCOpalExamples new.
	
	aCompiledMethod := ir compiledMethod.
	self assert: (aCompiledMethod valueWithReceiver: instance arguments: #()) = instance exampleReturn1plus2.
	
	