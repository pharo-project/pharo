tests - examples
testDecompileBytecodeDecompilerTestClass
	| decompiledIR aCompiledMethod |
	self class methods
		do: [ :each | 
			decompiledIR := IRBytecodeDecompiler new decompile: each.
			aCompiledMethod := decompiledIR compiledMethod ]