tests - examples
testWhileTrue
	| cm decompiledIR aCompiledMethod |
	cm := self class >> #testWhileTrue.
	decompiledIR := IRBytecodeDecompiler new decompile: cm.
	aCompiledMethod := decompiledIR compiledMethod