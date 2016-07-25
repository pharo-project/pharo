tests-blocks
testExampleSimpleBlockLocal
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleSimpleBlockLocal) parseTree generate.
	instance := OCOpalExamples new.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) = instance exampleSimpleBlockLocal