tests-blocks-optimized
testExampleMethodWithOptimizedBlocksA
	| ir method newMethod instance |
	
	method := (OCOpalExamples>>#exampleMethodWithOptimizedBlocksA) parseTree generate.
	instance := OCOpalExamples new.
	
	ir := IRBytecodeDecompiler new decompile: method.
	newMethod := ir compiledMethod.
	
	self assert: (newMethod valueWithReceiver: instance arguments: #()) = instance exampleMethodWithOptimizedBlocksA