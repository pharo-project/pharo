tests-variables
testExampleThisContext
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleThisContext) parseTree generate.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) isContext