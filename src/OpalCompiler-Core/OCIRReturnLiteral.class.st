"
specific node when returning a constant
"
Class {
	#name : 'OCIRReturnLiteral',
	#superclass : 'OCIRReturn',
	#instVars : [
		'literal'
	],
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
OCIRReturnLiteral >> accept: aVisitor [
	^ aVisitor visitReturnLiteral: self
]

{ #category : 'accessing' }
OCIRReturnLiteral >> literal [
	^ literal
]

{ #category : 'accessing' }
OCIRReturnLiteral >> literal: anObject [
	literal := anObject
]
