tests - examples
testDecompileIRBuilderTestClass
	| decompiledIR aCompiledMethod |
	IRBuilderTest methods
		do: [ :each | 
			decompiledIR := IRBytecodeDecompiler new decompile: each.
			aCompiledMethod := decompiledIR compiledMethod ]