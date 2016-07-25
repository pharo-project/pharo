tests-blocks-optimized
testExampleToDoOutsideTemp
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleToDoOutsideTemp) parseTree generate.
	instance := OCOpalExamples new.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) = instance exampleToDoOutsideTemp