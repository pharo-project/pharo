tests-simple
testExampleReturn1plus2
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleReturn1plus2) parseTree generate.
	instance := OCOpalExamples new.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) = instance exampleReturn1plus2