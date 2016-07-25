tests-blocks
testExampleBlockExternal2
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleBlockExternal2) parseTree generate.
	instance := OCOpalExamples new.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) = instance exampleBlockExternal2.
