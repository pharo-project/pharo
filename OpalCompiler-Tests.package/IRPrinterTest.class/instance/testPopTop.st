testing
testPopTop
	| ir |
	ir := IRBuilderTest new testPopTop.
	self assert: ir longPrintString =  '
label: 1
pushReceiver
pushLiteral: false
popTop
returnTop
'