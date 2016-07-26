testing
testRemoteTempNested
	| ir |
	ir := IRBuilderTest new testRemoteTempNested.
	self assert: ir longPrintString =   '
label: 1
createTempVectorNamed: #methodVector withVars: #(#b)
pushLiteral: 1
popIntoTemp: #a
pushClosureCopyCopiedValues: #(#methodVector #a) args: #()

label: 2
createTempVectorNamed: #blockVector withVars: #(#f)
pushTemp: #a
pushClosureCopyCopiedValues: #(#methodVector) args: #()

label: 3
pushLiteral: 1
storeRemoteTemp: #b inVector: #methodVector
blockReturnTop

label: 4
send: #value
send: #+
storeRemoteTemp: #b inVector: #methodVector
blockReturnTop

label: 5
send: #value
pushRemoteTemp: #b inVector: #methodVector
returnTop
'