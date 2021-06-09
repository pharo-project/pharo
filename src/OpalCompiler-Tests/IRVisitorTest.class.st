Class {
	#name : #IRVisitorTest,
	#superclass : #TestCase,
	#category : #'OpalCompiler-Tests-IR'
}

{ #category : #utilities }
IRVisitorTest >> interpret: ir [
	IRVisitor new visitNode: ir
]

{ #category : #tests }
IRVisitorTest >> testDup [
	| ir |
	ir := IRBuilderTest new testDup.
	self interpret: ir
]

{ #category : #tests }
IRVisitorTest >> testInstVar [
	| ir |
	ir := IRBuilderTest new testInstVar.
	self interpret: ir
]

{ #category : #tests }
IRVisitorTest >> testJumpAheadTo [
	| ir |
	ir := IRBuilderTest new testJumpAheadTo.
	self interpret: ir
]

{ #category : #tests }
IRVisitorTest >> testJumpAheadToIf [
	| ir |
	ir := IRBuilderTest new testJumpAheadToIf.
	self interpret: ir
]

{ #category : #tests }
IRVisitorTest >> testJumpBackTo [
	| ir |
	ir := IRBuilderTest new testJumpBackTo.
	self interpret: ir
]

{ #category : #tests }
IRVisitorTest >> testLiteralArray [
	| ir |
	ir := IRBuilderTest new testLiteralArray.
	self interpret: ir
]

{ #category : #tests }
IRVisitorTest >> testLiteralVariableClass [
	| ir |
	ir := IRBuilderTest new testLiteralVariableClass.
	self interpret: ir
]

{ #category : #tests }
IRVisitorTest >> testPopTop [
	| ir |
	ir := IRBuilderTest new testPopTop.
	self interpret: ir
]

{ #category : #tests }
IRVisitorTest >> testPushClosureCopyNoCopied [
	| ir |
	ir := IRBuilderTest new testPushClosureCopyNoCopied.
	self interpret: ir
]

{ #category : #tests }
IRVisitorTest >> testPushConsArray [
	| ir |
	ir := IRBuilderTest new testPushConsArray.
	self interpret: ir
]

{ #category : #tests }
IRVisitorTest >> testPushNewArray [
	| ir |
	ir := IRBuilderTest new testPushNewArray.
	self interpret: ir
]

{ #category : #tests }
IRVisitorTest >> testPushSelf [
	| ir |
	ir := IRBuilderTest new testPushSelf.
	self interpret: ir
]

{ #category : #tests }
IRVisitorTest >> testPushTempArgument [
	| ir |
	ir := IRBuilderTest new testPushTempArgument.
	self interpret: ir
]

{ #category : #tests }
IRVisitorTest >> testPushTempTemp [
	| ir |
	ir := IRBuilderTest new testPushTempTemp.
	self interpret: ir
]

{ #category : #tests }
IRVisitorTest >> testPushThisContext [
	| ir |
	ir := IRBuilderTest new testPushThisContext.
	self interpret: ir
]

{ #category : #tests }
IRVisitorTest >> testRemoteTemp [
	| ir |
	ir := IRBuilderTest new testRemoteTemp.
	self interpret: ir
]

{ #category : #tests }
IRVisitorTest >> testRemoteTempNested [
	| ir |
	ir := IRBuilderTest new testRemoteTempNested.
	self interpret: ir
]

{ #category : #tests }
IRVisitorTest >> testReturnTop [
	| ir |
	ir := IRBuilderTest new testReturnTop.
	self interpret: ir
]

{ #category : #tests }
IRVisitorTest >> testSendSuper [
	| ir |
	ir := IRBuilderTest new testSendSuper.
	self interpret: ir
]

{ #category : #tests }
IRVisitorTest >> testStoreIntoVariable [
	| ir |
	ir := IRBuilderTest new testStoreIntoVariable.
	self interpret: ir
]

{ #category : #tests }
IRVisitorTest >> testStoreTemp [
	| ir |
	ir := IRBuilderTest new testStoreTemp.
	self interpret: ir
]
