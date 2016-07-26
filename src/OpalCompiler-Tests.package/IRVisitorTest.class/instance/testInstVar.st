testing
testInstVar
	| ir |
	ir := IRBuilderTest new testInstVar.
	self interpret: ir