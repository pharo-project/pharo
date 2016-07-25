testing - variables
testExampleThisContext
	| ir ast aCompiledMethod instance |
	ast := (OCOpalExamples>>#exampleThisContext) parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	instance := OCOpalExamples new.
	
	aCompiledMethod := ir compiledMethod.
	self assert: OCOpalExamples new exampleThisContext isContext.
	self assert: ((OCOpalExamples>>#exampleThisContext) valueWithReceiver: instance arguments: #()) isContext.
	self assert: (aCompiledMethod valueWithReceiver: instance arguments: #()) isContext.
	
	