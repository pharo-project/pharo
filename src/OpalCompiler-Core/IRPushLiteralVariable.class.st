"
push literal variable
"
Class {
	#name : #IRPushLiteralVariable,
	#superclass : #IRLiteralVariableAccess,
	#category : 'OpalCompiler-Core-IR-Nodes'
}

{ #category : #visiting }
IRPushLiteralVariable >> accept: aVisitor [
	^ aVisitor visitPushLiteralVariable: self
]
