testing - misc
testTodoValue
	| ast ir aCompiledMethod instance |
	ast := (OCOpalExamples>>#exampleToDoValue) parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	instance := OCOpalExamples new .
	aCompiledMethod := ir compiledMethod.
	self assert: (aCompiledMethod valueWithReceiver: instance arguments: #()) = instance exampleToDoValue.