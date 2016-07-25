testing
testPushNewArray
	| ir |
	ir := IRBuilderTest new testPushNewArray.
	self interpret: ir