tests-blocks-optimized
testExampleIfTrue
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleIfTrue) parseTree generate.
	instance := OCOpalExamples new.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) = instance exampleIfTrue