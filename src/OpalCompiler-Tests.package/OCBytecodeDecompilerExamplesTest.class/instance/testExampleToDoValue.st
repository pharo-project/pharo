tests-misc
testExampleToDoValue
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleToDoValue) parseTree generate.
	instance := OCOpalExamples new.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) = instance exampleToDoValue