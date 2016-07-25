tests
testInstVar
	| ir1 ir2 method method2 |
	ir1 := IRBuilderTest new testInstVar.
	method := ir1 compiledMethod.
	ir2 := method decompileIR.
	method2 := ir2 compiledMethod.
	self deny: method2 == method.
	self assert: ir2 compiledMethod symbolic equals: method symbolic.
	self assert: method2 equals: method