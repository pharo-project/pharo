tests-blocks-optimized
testExampleWhileModificationBeforeNotInlined
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleWhileModificationBeforeNotInlined) parseTree generate.
	instance := OCOpalExamples new.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) = instance exampleWhileModificationBeforeNotInlined