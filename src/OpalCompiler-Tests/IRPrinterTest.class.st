Class {
	#name : 'IRPrinterTest',
	#superclass : 'TestCase',
	#category : 'OpalCompiler-Tests-IR',
	#package : 'OpalCompiler-Tests',
	#tag : 'IR'
}

{ #category : 'tests' }
IRPrinterTest >> testDup [
	| ir |
	ir := IRBuilderTest new testDup.
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
IRPrinterTest >> testInstVar [
	| ir |
	ir := IRBuilderTest new testInstVar.
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
IRPrinterTest >> testJumpAheadTo [
	| ir |
	ir := IRBuilderTest new testJumpAheadTo.
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
IRPrinterTest >> testJumpAheadToIf [
	| ir |
	ir := IRBuilderTest new testJumpAheadToIf.
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
IRPrinterTest >> testJumpBackTo [
	| ir |
	ir := IRBuilderTest new testJumpBackTo.
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
IRPrinterTest >> testLiteralArray [
	| ir |
	ir := IRBuilderTest new testLiteralArray.
	self
		assert: ir longPrintString
		equals:
			'
label: 1
returnLiteral: #(#test 4 #you)
'
]

{ #category : 'tests' }
IRPrinterTest >> testLiteralVariableClass [
	| ir |
	ir := IRBuilderTest new testLiteralVariableClass.
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
IRPrinterTest >> testPopTop [
	| ir |
	ir := IRBuilderTest new testPopTop.
	self
		assert: ir longPrintString
		equals:
			'
label: 1
returnReceiver
'
]

{ #category : 'tests' }
IRPrinterTest >> testPushConsArray [
	| ir |
	ir := IRBuilderTest new testPushConsArray.
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
IRPrinterTest >> testPushNewArray [
	| ir |
	ir := IRBuilderTest new testPushNewArray.
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
IRPrinterTest >> testPushSelf [
	| ir |
	ir := IRBuilderTest new testPushSelf.
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
IRPrinterTest >> testPushTempArgument [
	| ir |
	ir := IRBuilderTest new testPushTempArgument.
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
IRPrinterTest >> testPushTempTemp [
	| ir |
	ir := IRBuilderTest new testPushTempTemp.
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
IRPrinterTest >> testPushThisContext [
	| ir |
	ir := IRBuilderTest new testPushThisContext.
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
IRPrinterTest >> testReturnTop [
	| ir |
	ir := IRBuilderTest new testReturnTop.
	self
		assert: ir longPrintString
		equals:
			'
label: 1
returnLiteral: false
'
]

{ #category : 'tests' }
IRPrinterTest >> testSendSuper [
	| ir |
	ir := IRBuilderTest new testSendSuper.
	self
		assert: ir longPrintString
		equals:
			'
label: 1
pushReceiver
send: #isThisEverCalled toSuperOf: IRBuilderTest
returnTop
'
]

{ #category : 'tests' }
IRPrinterTest >> testStoreIntoVariable [
	| ir |
	ir := IRBuilderTest new testStoreIntoVariable.
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
IRPrinterTest >> testStoreTemp [
	| ir |
	ir := IRBuilderTest new testStoreTemp.
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
