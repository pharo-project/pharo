tests-blocks
testExampleSimpleBlockLocalNested
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleSimpleBlockNested) parseTree generate.
	instance := OCOpalExamples new.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) = instance exampleSimpleBlockNested