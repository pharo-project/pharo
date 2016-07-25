tests-simple
testExampleNewArray
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleNewArray) parseTree generate.
	instance := OCOpalExamples new.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) = instance exampleNewArray