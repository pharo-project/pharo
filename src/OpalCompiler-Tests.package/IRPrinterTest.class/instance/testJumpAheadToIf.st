testing
testJumpAheadToIf
	| ir |
	ir := IRBuilderTest new testJumpAheadToIf.
	self assert: ir longPrintString =  '
label: 1
pushReceiver
pushLiteral: true
if: true goto: 3 else: 2

label: 2
returnTop

label: 3
returnTop
'