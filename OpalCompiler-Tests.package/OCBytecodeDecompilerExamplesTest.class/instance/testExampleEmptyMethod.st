tests-simple
testExampleEmptyMethod
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleEmptyMethod) parseTree generate.
	instance := OCOpalExamples new.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) = instance exampleEmptyMethod.