tests-blocks-optimized
testExampleSimpleBlockLocalIf
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleSimpleBlockLocalIf) parseTree generate.
	instance := OCOpalExamples new.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) = instance exampleSimpleBlockLocalIf