tests-blocks-optimized
testExampleToDoInsideTempNotInlined
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleToDoInsideTempNotInlined) parseTree generate.
	instance := OCOpalExamples new.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) = instance exampleToDoInsideTempNotInlined