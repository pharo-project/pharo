Class {
	#name : 'OCIRVisitorTest',
	#superclass : 'TestCase',
	#category : 'OpalCompiler-Tests-IR',
	#package : 'OpalCompiler-Tests',
	#tag : 'IR'
}

{ #category : 'utilities' }
OCIRVisitorTest >> interpret: ir [
	OCIRVisitor new visitNode: ir
]

{ #category : 'tests' }
OCIRVisitorTest >> testDup [
	| ir |
	ir := OCIRBuilderTest new testDup.
	self interpret: ir
]

{ #category : 'tests' }
OCIRVisitorTest >> testInstVar [
	| ir |
	ir := OCIRBuilderTest new testInstVar.
	self interpret: ir
]

{ #category : 'tests' }
OCIRVisitorTest >> testJumpAheadTo [
	| ir |
	ir := OCIRBuilderTest new testJumpAheadTo.
	self interpret: ir
]

{ #category : 'tests' }
OCIRVisitorTest >> testJumpAheadToIf [
	| ir |
	ir := OCIRBuilderTest new testJumpAheadToIf.
	self interpret: ir
]

{ #category : 'tests' }
OCIRVisitorTest >> testJumpBackTo [
	| ir |
	ir := OCIRBuilderTest new testJumpBackTo.
	self interpret: ir
]

{ #category : 'tests' }
OCIRVisitorTest >> testLiteralArray [
	| ir |
	ir := OCIRBuilderTest new testLiteralArray.
	self interpret: ir
]

{ #category : 'tests' }
OCIRVisitorTest >> testLiteralVariableClass [
	| ir |
	ir := OCIRBuilderTest new testLiteralVariableClass.
	self interpret: ir
]

{ #category : 'tests' }
OCIRVisitorTest >> testPopTop [
	| ir |
	ir := OCIRBuilderTest new testPopTop.
	self interpret: ir
]

{ #category : 'tests' }
OCIRVisitorTest >> testPushConsArray [
	| ir |
	ir := OCIRBuilderTest new testPushConsArray.
	self interpret: ir
]

{ #category : 'tests' }
OCIRVisitorTest >> testPushNewArray [
	| ir |
	ir := OCIRBuilderTest new testPushNewArray.
	self interpret: ir
]

{ #category : 'tests' }
OCIRVisitorTest >> testPushSelf [
	| ir |
	ir := OCIRBuilderTest new testPushSelf.
	self interpret: ir
]

{ #category : 'tests' }
OCIRVisitorTest >> testPushTempArgument [
	| ir |
	ir := OCIRBuilderTest new testPushTempArgument.
	self interpret: ir
]

{ #category : 'tests' }
OCIRVisitorTest >> testPushTempTemp [
	| ir |
	ir := OCIRBuilderTest new testPushTempTemp.
	self interpret: ir
]

{ #category : 'tests' }
OCIRVisitorTest >> testPushThisContext [
	| ir |
	ir := OCIRBuilderTest new testPushThisContext.
	self interpret: ir
]

{ #category : 'tests' }
OCIRVisitorTest >> testReturnTop [
	| ir |
	ir := OCIRBuilderTest new testReturnTop.
	self interpret: ir
]

{ #category : 'tests' }
OCIRVisitorTest >> testSendSuper [
	| ir |
	ir := OCIRBuilderTest new testSendSuper.
	self interpret: ir
]

{ #category : 'tests' }
OCIRVisitorTest >> testStoreIntoVariable [
	| ir |
	ir := OCIRBuilderTest new testStoreIntoVariable.
	self interpret: ir
]

{ #category : 'tests' }
OCIRVisitorTest >> testStoreTemp [
	| ir |
	ir := OCIRBuilderTest new testStoreTemp.
	self interpret: ir
]
