tests
testLiteralVariableGlobale
	| ir1 ir2 method |
	ir1 := IRBuilderTest new testLiteralVariableGlobale.
	method := ir1 compiledMethod.
	ir2 := IRBytecodeDecompiler new decompile: method.
	self deny: ir2 compiledMethod == method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: ir2 compiledMethod equals: method