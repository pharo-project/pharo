tests-blocks
testExampleSimpleBlockArgument1
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleSimpleBlockArgument1) parseTree generate.
	instance := OCOpalExamples new.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) = instance exampleSimpleBlockArgument1