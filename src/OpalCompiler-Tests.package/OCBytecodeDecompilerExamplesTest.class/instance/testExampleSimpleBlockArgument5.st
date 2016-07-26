tests-blocks
testExampleSimpleBlockArgument5
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleSimpleBlockArgument5) parseTree generate.
	instance := OCOpalExamples new.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) = instance exampleSimpleBlockArgument5