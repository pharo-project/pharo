testing
testStoreIntoVariable
	| ir |
	ir := IRBuilderTest new testStoreIntoVariable.
	self assert: ir longPrintString =  '
label: 1
pushLiteral: 4
storeLiteralVariable: TestToPush
returnTop
'