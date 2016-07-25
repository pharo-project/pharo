tests-blocks
testExampleSimpleBlockArgument4
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleSimpleBlockArgument4) parseTree generate.
	instance := OCOpalExamples new.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) = instance exampleSimpleBlockArgument4