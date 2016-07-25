testing
testRemoteTempNested
	| ir |
	ir := IRBuilderTest new testRemoteTempNested.
	self interpret: ir