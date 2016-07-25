testing
testJumpAheadTo
	| ir |
	ir := IRBuilderTest new testJumpAheadTo.
	self assert: ir longPrintString =  '
label: 1
pushLiteral: 2
pushLiteral: 1
send: #+
goto: 2

label: 2
returnTop
'