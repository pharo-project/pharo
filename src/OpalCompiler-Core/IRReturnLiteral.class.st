"
specific node when returning a constant
"
Class {
	#name : 'IRReturnLiteral',
	#superclass : 'IRReturn',
	#instVars : [
		'literal'
	],
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
IRReturnLiteral >> accept: aVisitor [
	^ aVisitor visitReturnLiteral: self
]

{ #category : 'accessing' }
IRReturnLiteral >> literal [
	^ literal
]

{ #category : 'accessing' }
IRReturnLiteral >> literal: anObject [
	literal := anObject
]
