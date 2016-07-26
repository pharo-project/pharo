testing
testStoreTemp
	| ir |
	ir := IRBuilderTest new testStoreTemp.
	self interpret: ir