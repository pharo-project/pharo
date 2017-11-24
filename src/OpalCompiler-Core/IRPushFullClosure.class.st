"
I represent the creation and the push on stack of a FullBlockClosure.


"
Class {
	#name : #IRPushFullClosure,
	#superclass : #IRInstruction,
	#instVars : [
		'compiledBlock',
		'copiedValues'
	],
	#category : #'OpalCompiler-Core-IR-Nodes'
}

{ #category : #visiting }
IRPushFullClosure >> accept: aVisitor [
	^ aVisitor visitPushFullClosure: self
]

{ #category : #accessing }
IRPushFullClosure >> compiledBlock [
	^ compiledBlock
]

{ #category : #accessing }
IRPushFullClosure >> compiledBlock: anObject [
	compiledBlock := anObject
]

{ #category : #accessing }
IRPushFullClosure >> copiedValues [
	^ copiedValues
]

{ #category : #accessing }
IRPushFullClosure >> copiedValues: anObject [
	copiedValues := anObject
]

{ #category : #debugging }
IRPushFullClosure >> indexForVarNamed: aName [
	^ sourceNode ir indexForVarNamed: aName
]
