testing
testStoreTemp
	| ir |
	ir := IRBuilderTest new testStoreTemp.
	self assert: ir longPrintString = '
label: 1
pushLiteral: 34
popIntoTemp: #a
pushTemp: #a
returnTop
'