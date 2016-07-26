testing
testPopTop
	| ir |
	ir := IRBuilderTest new testPopTop.
	self interpret: ir