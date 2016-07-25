testing
testStoreIntoVariable
	| ir |
	ir := IRBuilderTest new testStoreIntoVariable.
	self interpret: ir