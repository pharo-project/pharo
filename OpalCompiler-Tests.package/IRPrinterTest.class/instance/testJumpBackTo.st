testing
testJumpBackTo
	| ir |
	ir := IRBuilderTest new testJumpBackTo.
	self assert: ir longPrintString =  '
label: 1
pushReceiver
pushLiteral: false
goto: 2

label: 2
if: true goto: 4 else: 3

label: 3
pushLiteral: true
goto: 2

label: 4
returnTop
'