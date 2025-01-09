Class {
	#name : 'OCIRPrinterTest',
	#superclass : 'TestCase',
	#category : 'OpalCompiler-Tests-IR',
	#package : 'OpalCompiler-Tests',
	#tag : 'IR'
}

{ #category : 'tests' }
OCIRPrinterTest >> testDup [
	| ir |
	ir := OCIRBuilderTest new testDup.
	self
		assert: ir longPrintString
		equals:
			'
label: 1
pushLiteral: 3
pushDup
send: #=
returnTop
'
]

{ #category : 'tests' }
OCIRPrinterTest >> testInstVar [
	| ir |
	ir := OCIRBuilderTest new testInstVar.
	self
		assert: ir longPrintString
		equals:
			'
label: 1
pushInstVar: 1
pushInstVar: 2
send: #+
returnTop
'
]

{ #category : 'tests' }
OCIRPrinterTest >> testJumpAheadTo [
	| ir |
	ir := OCIRBuilderTest new testJumpAheadTo.
	self
		assert: ir longPrintString
		equals:
			'
label: 1
pushLiteral: 2
pushLiteral: 1
send: #+
goto: 2

label: 2
returnTop
'
]

{ #category : 'tests' }
OCIRPrinterTest >> testJumpAheadToIf [
	| ir |
	ir := OCIRBuilderTest new testJumpAheadToIf.
	self
		assert: ir longPrintString
		equals:
			'
label: 1
pushReceiver
pushLiteral: true
if: true goto: 3 else: 2

label: 2
returnTop

label: 3
returnTop
'
]

{ #category : 'tests' }
OCIRPrinterTest >> testJumpBackTo [
	| ir |
	ir := OCIRBuilderTest new testJumpBackTo.
	self
		assert: ir longPrintString
		equals:
			'
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
]

{ #category : 'tests' }
OCIRPrinterTest >> testLiteralArray [
	| ir |
	ir := OCIRBuilderTest new testLiteralArray.
	self
		assert: ir longPrintString
		equals:
			'
label: 1
returnLiteral: #(#test 4 #you)
'
]

{ #category : 'tests' }
OCIRPrinterTest >> testLiteralVariableClass [
	| ir |
	ir := OCIRBuilderTest new testLiteralVariableClass.
	self
		assert: ir longPrintString
		equals:
			'
label: 1
pushLiteralVariable: Object
returnTop
'
]

{ #category : 'tests' }
OCIRPrinterTest >> testPopTop [
	| ir |
	ir := OCIRBuilderTest new testPopTop.
	self
		assert: ir longPrintString
		equals:
			'
label: 1
returnReceiver
'
]

{ #category : 'tests' }
OCIRPrinterTest >> testPushConsArray [
	| ir |
	ir := OCIRBuilderTest new testPushConsArray.
	self
		assert: ir longPrintString
		equals:
			'
label: 1
pushReceiver
pushConsArray: 1
returnTop
'
]

{ #category : 'tests' }
OCIRPrinterTest >> testPushNewArray [
	| ir |
	ir := OCIRBuilderTest new testPushNewArray.
	self
		assert: ir longPrintString
		equals:
			'
label: 1
pushNewArray: 1
returnTop
'
]

{ #category : 'tests' }
OCIRPrinterTest >> testPushSelf [
	| ir |
	ir := OCIRBuilderTest new testPushSelf.
	self
		assert: ir longPrintString
		equals:
			'
label: 1
pushReceiver
send: #class
returnTop
'
]

{ #category : 'tests' }
OCIRPrinterTest >> testPushTempArgument [
	| ir |
	ir := OCIRBuilderTest new testPushTempArgument.
	self
		assert: ir longPrintString
		equals:
			'
label: 1
pushTemp: #a
pushTemp: #b
send: #+
returnTop
'
]

{ #category : 'tests' }
OCIRPrinterTest >> testPushTempTemp [
	| ir |
	ir := OCIRBuilderTest new testPushTempTemp.
	self
		assert: ir longPrintString
		equals:
			'
label: 1
pushTemp: #a
returnTop
'
]

{ #category : 'tests' }
OCIRPrinterTest >> testPushThisContext [
	| ir |
	ir := OCIRBuilderTest new testPushThisContext.
	self
		assert: ir longPrintString
		equals:
			'
label: 1
pushThisContext
send: #receiver
returnTop
'
]

{ #category : 'tests' }
OCIRPrinterTest >> testReturnTop [
	| ir |
	ir := OCIRBuilderTest new testReturnTop.
	self
		assert: ir longPrintString
		equals:
			'
label: 1
returnLiteral: false
'
]

{ #category : 'tests' }
OCIRPrinterTest >> testSendSuper [
	| ir |
	ir := OCIRBuilderTest new testSendSuper.
	self
		assert: ir longPrintString
		equals:
			'
label: 1
pushReceiver
send: #isThisEverCalled toSuperOf: OCIRBuilderTest
returnTop
'
]

{ #category : 'tests' }
OCIRPrinterTest >> testStoreIntoVariable [
	| ir |
	ir := OCIRBuilderTest new testStoreIntoVariable.
	self
		assert: ir longPrintString
		equals:
			'
label: 1
pushLiteral: 4
storeLiteralVariable: TestToPush
returnTop
'
]

{ #category : 'tests' }
OCIRPrinterTest >> testStoreTemp [
	| ir |
	ir := OCIRBuilderTest new testStoreTemp.
	self
		assert: ir longPrintString
		equals:
			'
label: 1
pushLiteral: 34
popIntoTemp: #a
pushTemp: #a
returnTop
'
]
