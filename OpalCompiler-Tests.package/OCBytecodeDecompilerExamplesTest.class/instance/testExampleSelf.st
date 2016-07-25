tests-variables
testExampleSelf
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleSelf) parseTree generate.
	instance := OCOpalExamples new.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) = instance exampleSelf