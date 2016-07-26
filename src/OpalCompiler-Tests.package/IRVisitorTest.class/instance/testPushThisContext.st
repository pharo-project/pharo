testing
testPushThisContext
	| ir |
	ir := IRBuilderTest new testPushThisContext.
	self interpret: ir