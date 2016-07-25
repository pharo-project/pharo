tests-variables
testExampleSuper
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleSuper) parseTree generate.
	instance := OCOpalExamples new.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) = instance exampleSuper