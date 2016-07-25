tests-blocks
testExampleSimpleBlockArgument2
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleSimpleBlockArgument2) parseTree generate.
	instance := OCOpalExamples new.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) = instance exampleSimpleBlockArgument2