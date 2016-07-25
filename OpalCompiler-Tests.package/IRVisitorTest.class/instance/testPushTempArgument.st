testing
testPushTempArgument
	| ir |
	ir := IRBuilderTest new testPushTempArgument.
	self interpret: ir