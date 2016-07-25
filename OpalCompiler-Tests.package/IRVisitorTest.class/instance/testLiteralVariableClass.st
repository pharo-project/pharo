testing
testLiteralVariableClass
	| ir |
	ir := IRBuilderTest new testLiteralVariableClass.
	self interpret: ir