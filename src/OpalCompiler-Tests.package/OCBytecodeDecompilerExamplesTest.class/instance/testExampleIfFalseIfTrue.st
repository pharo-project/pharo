tests-blocks-optimized
testExampleIfFalseIfTrue
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleIfFalseIfTrue) parseTree generate.
	instance := OCOpalExamples new.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) = instance exampleIfFalseIfTrue.