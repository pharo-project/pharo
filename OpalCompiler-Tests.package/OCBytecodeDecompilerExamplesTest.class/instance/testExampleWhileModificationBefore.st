tests-blocks-optimized
testExampleWhileModificationBefore
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleWhileModificationBefore) parseTree generate.
	instance := OCOpalExamples new.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) = instance exampleWhileModificationBefore