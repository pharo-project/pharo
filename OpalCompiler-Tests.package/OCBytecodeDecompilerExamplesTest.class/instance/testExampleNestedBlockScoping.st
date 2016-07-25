tests-blocks
testExampleNestedBlockScoping
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleNestedBlockScoping) parseTree generate.
	instance := OCOpalExamples new.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) = instance exampleNestedBlockScoping