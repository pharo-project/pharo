tests-blocks
testExampleSimpleBlock
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleSimpleBlock) parseTree generate.
	instance := OCOpalExamples new.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) value = instance exampleSimpleBlock value