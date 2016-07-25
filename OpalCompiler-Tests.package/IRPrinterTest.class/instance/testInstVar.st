testing
testInstVar
	| ir |
	ir := IRBuilderTest new testInstVar.
	self assert: ir longPrintString = 
 '
label: 1
pushInstVar: 1
pushInstVar: 2
send: #+
returnTop
'