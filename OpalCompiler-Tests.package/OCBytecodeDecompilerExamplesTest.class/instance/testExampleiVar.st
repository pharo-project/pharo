tests-variables
testExampleiVar
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleiVar) parseTree generate.
	instance := OCOpalExamples new.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) = instance exampleiVar