testing
testDup
	| ir |
	ir := IRBuilderTest new testDup.
	self assert: ir longPrintString = '
label: 1
pushLiteral: 3
pushDup
send: #=
returnTop
'