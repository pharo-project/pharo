testing - primitives
testExamplePrimitiveErrorCodeModule
	| method ast ir newMethod |
	method := (OCOpalExamples>>#examplePrimitiveErrorCodeModule).
	ast := method parseTree.
	ir := (ast doSemanticAnalysisIn: OCOpalExamples) ir.

	newMethod := ir compiledMethod.
	
	self assert: method primitive = newMethod primitive. 
	self assert: method pragmas printString  = newMethod pragmas printString
	

	
	

	