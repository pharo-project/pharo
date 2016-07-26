tests-blocks-optimized
testExampleIfNotNilArg
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleIfNotNilArg) parseTree generate.
	instance := OCOpalExamples new.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) = instance exampleIfNotNilArg.