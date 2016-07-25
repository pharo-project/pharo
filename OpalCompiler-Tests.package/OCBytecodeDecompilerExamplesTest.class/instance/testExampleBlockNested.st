tests-blocks
testExampleBlockNested
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleBlockNested) parseTree generate.
	instance := OCOpalExamples new.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) = instance exampleBlockNested.