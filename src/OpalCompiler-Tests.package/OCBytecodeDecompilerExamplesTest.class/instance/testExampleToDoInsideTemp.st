tests-blocks-optimized
testExampleToDoInsideTemp
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleToDoInsideTemp) parseTree generate.
	instance := OCOpalExamples new.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) = instance exampleToDoInsideTemp