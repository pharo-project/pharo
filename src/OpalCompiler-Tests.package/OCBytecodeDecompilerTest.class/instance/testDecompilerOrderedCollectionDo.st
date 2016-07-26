tests - examples
testDecompilerOrderedCollectionDo
	| cm decompiledIR |
	cm := OrderedCollection >> #do:.
	decompiledIR := IRBytecodeDecompiler new decompile: cm.
	decompiledIR compiledMethod