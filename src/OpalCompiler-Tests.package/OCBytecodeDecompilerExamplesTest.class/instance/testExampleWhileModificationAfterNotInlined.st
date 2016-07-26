tests-blocks-optimized
testExampleWhileModificationAfterNotInlined
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleWhileModificationAfterNotInlined) parseTree generate.
	instance := OCOpalExamples new.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) = instance exampleWhileModificationAfterNotInlined