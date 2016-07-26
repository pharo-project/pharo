tests-blocks-optimized
testExampleIfTrueIfFalse
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleIfTrueIfFalse) parseTree generate.
	instance := OCOpalExamples new.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) = instance exampleIfTrueIfFalse