Class {
	#name : #IRVisitorTest,
	#superclass : #TestCase,
	#category : 'OpalCompiler-Tests-IR'
}

{ #category : #helper }
IRVisitorTest >> interpret: ir [
	IRVisitor new visitNode: ir
]

{ #category : #testing }
IRVisitorTest >> testDup [
	| ir |
	ir := IRBuilderTest new testDup.
	self interpret: ir
]

{ #category : #testing }
IRVisitorTest >> testInstVar [
	| ir |
	ir := IRBuilderTest new testInstVar.
	self interpret: ir
]

{ #category : #testing }
IRVisitorTest >> testJumpAheadTo [
	| ir |
	ir := IRBuilderTest new testJumpAheadTo.
	self interpret: ir
]

{ #category : #testing }
IRVisitorTest >> testJumpAheadToIf [
	| ir |
	ir := IRBuilderTest new testJumpAheadToIf.
	self interpret: ir
]

{ #category : #testing }
IRVisitorTest >> testJumpBackTo [
	| ir |
	ir := IRBuilderTest new testJumpBackTo.
	self interpret: ir
]

{ #category : #testing }
IRVisitorTest >> testLiteralArray [
	| ir |
	ir := IRBuilderTest new testLiteralArray.
	self interpret: ir
]

{ #category : #testing }
IRVisitorTest >> testLiteralVariableClass [
	| ir |
	ir := IRBuilderTest new testLiteralVariableClass.
	self interpret: ir
]

{ #category : #testing }
IRVisitorTest >> testPopTop [
	| ir |
	ir := IRBuilderTest new testPopTop.
	self interpret: ir
]

{ #category : #testing }
IRVisitorTest >> testPushClosureCopyNoCopied [
	| ir |
	ir := IRBuilderTest new testPushClosureCopyNoCopied.
	self interpret: ir
]

{ #category : #testing }
IRVisitorTest >> testPushConsArray [
	| ir |
	ir := IRBuilderTest new testPushConsArray.
	self interpret: ir
]

{ #category : #testing }
IRVisitorTest >> testPushNewArray [
	| ir |
	ir := IRBuilderTest new testPushNewArray.
	self interpret: ir
]

{ #category : #testing }
IRVisitorTest >> testPushSelf [
	| ir |
	ir := IRBuilderTest new testPushSelf.
	self interpret: ir
]

{ #category : #testing }
IRVisitorTest >> testPushTempArgument [
	| ir |
	ir := IRBuilderTest new testPushTempArgument.
	self interpret: ir
]

{ #category : #testing }
IRVisitorTest >> testPushTempTemp [
	| ir |
	ir := IRBuilderTest new testPushTempTemp.
	self interpret: ir
]

{ #category : #testing }
IRVisitorTest >> testPushThisContext [
	| ir |
	ir := IRBuilderTest new testPushThisContext.
	self interpret: ir
]

{ #category : #testing }
IRVisitorTest >> testRemoteTemp [
	| ir |
	ir := IRBuilderTest new testRemoteTemp.
	self interpret: ir
]

{ #category : #testing }
IRVisitorTest >> testRemoteTempNested [
	| ir |
	ir := IRBuilderTest new testRemoteTempNested.
	self interpret: ir
]

{ #category : #testing }
IRVisitorTest >> testReturnTop [
	| ir |
	ir := IRBuilderTest new testReturnTop.
	self interpret: ir
]

{ #category : #testing }
IRVisitorTest >> testSendSuper [
	| ir |
	ir := IRBuilderTest new testSendSuper.
	self interpret: ir
]

{ #category : #testing }
IRVisitorTest >> testStoreIntoVariable [
	| ir |
	ir := IRBuilderTest new testStoreIntoVariable.
	self interpret: ir
]

{ #category : #testing }
IRVisitorTest >> testStoreTemp [
	| ir |
	ir := IRBuilderTest new testStoreTemp.
	self interpret: ir
]
