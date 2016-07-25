testing - primitives
testExamplePrimitiveErrorCode
	| method ast ir newMethod |
	method := (OCOpalExamples>>#examplePrimitiveErrorCode).
	ast := method parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.

	newMethod := ir compiledMethod.
	self assert: method primitive = newMethod primitive. 

	

	
	

	