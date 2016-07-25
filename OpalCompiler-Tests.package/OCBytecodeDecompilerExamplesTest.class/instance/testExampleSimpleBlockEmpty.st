tests-blocks
testExampleSimpleBlockEmpty
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleSimpleBlockEmpty) parseTree generate.
	instance := OCOpalExamples new.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) = instance exampleSimpleBlockEmpty