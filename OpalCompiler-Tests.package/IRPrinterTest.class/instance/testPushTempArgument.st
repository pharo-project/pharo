testing
testPushTempArgument
	| ir |
	ir := IRBuilderTest new testPushTempArgument.
	self assert: ir longPrintString = '
label: 1
pushTemp: #a
pushTemp: #b
send: #+
returnTop
'