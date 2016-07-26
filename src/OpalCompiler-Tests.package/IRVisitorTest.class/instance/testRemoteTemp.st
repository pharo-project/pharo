testing
testRemoteTemp
	| ir |
	ir := IRBuilderTest new testRemoteTemp.
	self interpret: ir