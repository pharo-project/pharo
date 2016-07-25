testing - variables
testiVar
	| ir ast aCompiledMethod instance |
	ast := (OCOpalExamples>>#exampleiVar) parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.
	instance := OCOpalExamples new.
	
	aCompiledMethod := ir compiledMethod.
	self assert: (aCompiledMethod valueWithReceiver: instance arguments: #()) = instance exampleiVar.
	
	