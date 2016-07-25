tests-blocks-optimized
testExampleToDoOutsideTempNotInlined
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleToDoOutsideTempNotInlined) parseTree generate.
	instance := OCOpalExamples new.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) = instance exampleToDoOutsideTempNotInlined