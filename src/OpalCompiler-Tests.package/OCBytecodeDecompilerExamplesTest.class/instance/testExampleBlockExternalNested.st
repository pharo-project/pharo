tests-blocks
testExampleBlockExternalNested
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleBlockExternalNested) parseTree generate.
	instance := OCOpalExamples new.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) = instance exampleBlockExternalNested.
