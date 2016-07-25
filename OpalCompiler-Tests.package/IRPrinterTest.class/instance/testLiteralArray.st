testing
testLiteralArray
	| ir |
	ir := IRBuilderTest new testLiteralArray.  
	self assert: ir longPrintString =  '
label: 1
returnLiteral: #(#test 4 #you)
'