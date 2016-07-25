tests-blocks-optimized
testExampleToDoArgumentNotInlined
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleToDoArgumentNotInlined) parseTree generate.
	instance := OCOpalExamples new.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) = instance exampleToDoArgumentNotInlined