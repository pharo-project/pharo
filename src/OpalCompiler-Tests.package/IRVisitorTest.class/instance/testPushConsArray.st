testing
testPushConsArray
	| ir |
	ir := IRBuilderTest new testPushConsArray.
	self interpret: ir