testing
testReturnTop
	| ir |
	ir := IRBuilderTest new testReturnTop.
	self interpret: ir