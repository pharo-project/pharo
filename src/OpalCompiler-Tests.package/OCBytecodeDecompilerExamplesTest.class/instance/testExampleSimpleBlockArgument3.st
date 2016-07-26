tests-blocks
testExampleSimpleBlockArgument3
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleSimpleBlockArgument3) parseTree generate.
	instance := OCOpalExamples new.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) = instance exampleSimpleBlockArgument3