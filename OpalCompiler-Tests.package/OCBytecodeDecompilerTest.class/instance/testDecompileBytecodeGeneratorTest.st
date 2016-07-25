tests - examples
testDecompileBytecodeGeneratorTest
	| decompiledIR aCompiledMethod |
	OCBytecodeGeneratorTest methods
		do: [ :each | 
			decompiledIR := IRBytecodeDecompiler new decompile: each.
			aCompiledMethod := decompiledIR compiledMethod ]