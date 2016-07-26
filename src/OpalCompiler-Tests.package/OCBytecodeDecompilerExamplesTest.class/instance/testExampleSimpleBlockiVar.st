tests-blocks
testExampleSimpleBlockiVar
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleSimpleBlockiVar) parseTree generate.
	instance := OCOpalExamples new.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) = instance exampleSimpleBlockiVar