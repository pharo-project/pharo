testing
testJumpAheadToIf
	| ir |
	ir := IRBuilderTest new testJumpAheadToIf.
	self interpret: ir