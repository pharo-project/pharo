tests-blocks
testExampleBlockInternal
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleBlockInternal) parseTree generate.
	instance := OCOpalExamples new.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) = instance exampleBlockInternal.