tests-blocks
testExampleBlockExternalArg
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleBlockExternalArg) parseTree generate.
	instance := OCOpalExamples new.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) = instance exampleBlockExternalArg.
