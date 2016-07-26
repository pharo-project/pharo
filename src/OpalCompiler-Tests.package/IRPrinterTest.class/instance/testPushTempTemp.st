testing
testPushTempTemp
	| ir |
	ir := IRBuilderTest new testPushTempTemp.
	self assert: ir longPrintString =  '
label: 1
pushTemp: #a
returnTop
'