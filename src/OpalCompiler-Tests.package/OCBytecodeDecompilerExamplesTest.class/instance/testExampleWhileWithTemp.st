tests-blocks-optimized
testExampleWhileWithTemp
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleWhileWithTemp) parseTree generate.
	instance := OCOpalExamples new.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) = instance exampleWhileWithTemp