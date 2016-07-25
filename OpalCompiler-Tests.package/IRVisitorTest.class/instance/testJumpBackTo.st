testing
testJumpBackTo
	| ir |
	ir := IRBuilderTest new testJumpBackTo.
	self interpret: ir