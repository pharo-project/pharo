testing
testPushSelf
	| ir |
	ir := IRBuilderTest new testPushSelf.
	self assert: ir longPrintString =  '
label: 1
pushReceiver
send: #class
returnTop
'