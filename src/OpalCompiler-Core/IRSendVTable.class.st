Class {
	#name : 'IRSendVTable',
	#superclass : 'IRInstruction',
	#instVars : [
		'index',
		'args'
	],
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
IRSendVTable >> accept: aVisitor [

	^ aVisitor visitSendVTable: self
]

{ #category : 'accessing' }
IRSendVTable >> args [
	^ args
]

{ #category : 'accessing' }
IRSendVTable >> args: anInteger [

	args := anInteger
]

{ #category : 'accessing' }
IRSendVTable >> index [
	^ index
]

{ #category : 'accessing' }
IRSendVTable >> index: anInteger [

	index := anInteger
]
