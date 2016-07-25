tests-blocks-optimized
testExampleIfFalse
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleIfFalse) parseTree generate.
	instance := OCOpalExamples new.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) = instance exampleIfFalse.