testing
testPushClosureCopyNoCopied
	| ir |
	ir := IRBuilderTest new testPushClosureCopyNoCopied.
	self assert: ir longPrintString =  '
label: 1
pushReceiver
pushClosureCopyCopiedValues: #() args: #()

label: 2
pushLiteral: 1
pushLiteral: 2
send: #+
blockReturnTop

label: 3
send: #value
returnTop
'