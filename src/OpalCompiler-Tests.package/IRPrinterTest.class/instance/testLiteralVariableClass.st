testing
testLiteralVariableClass
	| ir |
	ir := IRBuilderTest new testLiteralVariableClass.
	self assert: ir longPrintString =  '
label: 1
pushLiteralVariable: Object
returnTop
'