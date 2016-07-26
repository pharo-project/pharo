testing
testPushConsArray
	| ir |
	ir := IRBuilderTest new testPushConsArray.
	self assert: ir longPrintString =  '
label: 1
pushReceiver
pushConsArray: 1
returnTop
'