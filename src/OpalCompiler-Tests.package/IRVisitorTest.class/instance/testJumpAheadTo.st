testing
testJumpAheadTo
	| ir |
	ir := IRBuilderTest new testJumpAheadTo.
	self interpret: ir