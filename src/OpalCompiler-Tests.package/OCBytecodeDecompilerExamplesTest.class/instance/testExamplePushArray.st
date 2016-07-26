tests-misc
testExamplePushArray
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#examplePushArray) parseTree generate.
	instance := OCOpalExamples new.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) = instance examplePushArray