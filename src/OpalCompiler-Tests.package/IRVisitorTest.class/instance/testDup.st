testing
testDup
	| ir |
	ir := IRBuilderTest new testDup.
	self interpret: ir