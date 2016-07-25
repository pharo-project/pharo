testing - misc
testTodoValueLimitExpression
	| ast ir aCompiledMethod instance |
	ast := (OCOpalExamples>>#exampleToDoValueLimitExpression) parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	instance := OCOpalExamples new .
	aCompiledMethod := ir compiledMethod.
	self assert: (aCompiledMethod valueWithReceiver: instance arguments: #()) = instance exampleToDoValueLimitExpression.