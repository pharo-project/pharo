testing
testPushNewArray
	| ir |
	ir := IRBuilderTest new testPushNewArray.
	self assert: ir longPrintString  =  '
label: 1
pushNewArray: 1
returnTop
'