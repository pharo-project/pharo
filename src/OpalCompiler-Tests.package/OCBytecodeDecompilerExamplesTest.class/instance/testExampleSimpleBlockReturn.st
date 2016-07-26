tests-blocks
testExampleSimpleBlockReturn
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleSimpleBlockReturn) parseTree generate.
	instance := OCOpalExamples new.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) = instance exampleSimpleBlockReturn