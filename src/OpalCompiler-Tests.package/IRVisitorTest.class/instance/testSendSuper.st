testing
testSendSuper
	| ir |
	ir := IRBuilderTest new testSendSuper.
	self interpret: ir