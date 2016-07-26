testing
testPushThisContext
	| ir |
	ir := IRBuilderTest new testPushThisContext.
	self assert: ir longPrintString =  '
label: 1
pushThisContext
send: #receiver
returnTop
'