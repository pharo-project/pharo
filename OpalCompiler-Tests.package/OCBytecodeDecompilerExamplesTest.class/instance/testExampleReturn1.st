tests-simple
testExampleReturn1
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleReturn1) parseTree generate.
	instance := OCOpalExamples new.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) = instance exampleReturn1