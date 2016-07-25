tests-blocks
testExampleMethodTempInNestedBlock
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleMethodTempInNestedBlock) parseTree generate.
	instance := OCOpalExamples new.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) = instance exampleMethodTempInNestedBlock