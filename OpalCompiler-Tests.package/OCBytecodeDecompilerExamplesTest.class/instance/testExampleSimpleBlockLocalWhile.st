tests-blocks-optimized
testExampleSimpleBlockLocalWhile
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleSimpleBlockLocalWhile) parseTree generate.
	instance := OCOpalExamples new.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) = instance exampleSimpleBlockLocalWhile