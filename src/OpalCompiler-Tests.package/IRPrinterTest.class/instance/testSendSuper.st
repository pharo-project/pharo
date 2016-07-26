testing
testSendSuper
	| ir |
	ir := IRBuilderTest new testSendSuper.
	self assert: ir longPrintString =  '
label: 1
pushReceiver
send: #isThisEverCalled toSuperOf: IRBuilderTest
returnTop
'