testing
testLiteralArray
	| ir |
	ir := IRBuilderTest new testLiteralArray.
	self interpret: ir