"
I represent the creation and the push on stack of a FullBlockClosure.


"
Class {
	#name : 'OCIRPushFullClosure',
	#superclass : 'OCIRInstruction',
	#instVars : [
		'compiledBlock',
		'copiedValues',
		'outerContextNeeded'
	],
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
OCIRPushFullClosure >> accept: aVisitor [
	^ aVisitor visitPushFullClosure: self
]

{ #category : 'accessing' }
OCIRPushFullClosure >> compiledBlock [
	^ compiledBlock
]

{ #category : 'accessing' }
OCIRPushFullClosure >> compiledBlock: anObject [
	compiledBlock := anObject
]

{ #category : 'accessing' }
OCIRPushFullClosure >> copiedValues [
	^ copiedValues
]

{ #category : 'accessing' }
OCIRPushFullClosure >> copiedValues: anObject [
	copiedValues := anObject
]

{ #category : 'debugging' }
OCIRPushFullClosure >> indexForVarNamed: aName [
	^ sourceNode ir indexForVarNamed: aName
]

{ #category : 'accessing' }
OCIRPushFullClosure >> outerContextNeeded [

	^ outerContextNeeded ifNil: [ true ]
]

{ #category : 'accessing' }
OCIRPushFullClosure >> outerContextNeeded: anObject [

	outerContextNeeded := anObject
]

{ #category : 'scoping' }
OCIRPushFullClosure >> remapCopiedValueAt: index oldOne: aTemp newOne: aRemote [

	self copiedValues at: index put: aRemote
]
