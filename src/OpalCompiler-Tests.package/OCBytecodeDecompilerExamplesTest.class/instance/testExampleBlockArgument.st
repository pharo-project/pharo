tests-blocks
testExampleBlockArgument
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleBlockArgument) parseTree generate.
	instance := OCOpalExamples new.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) = instance exampleBlockArgument.
