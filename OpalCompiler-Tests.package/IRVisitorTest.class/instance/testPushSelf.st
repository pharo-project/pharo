testing
testPushSelf
	| ir |
	ir := IRBuilderTest new testPushSelf.
	self interpret: ir