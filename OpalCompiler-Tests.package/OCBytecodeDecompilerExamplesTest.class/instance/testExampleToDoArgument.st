tests-blocks-optimized
testExampleToDoArgument
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleToDoArgument) parseTree generate.
	instance := OCOpalExamples new.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) = instance exampleToDoArgument