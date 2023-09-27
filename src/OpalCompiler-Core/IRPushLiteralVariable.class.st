"
push literal variable
"
Class {
	#name : 'IRPushLiteralVariable',
	#superclass : 'IRLiteralVariableAccess',
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
IRPushLiteralVariable >> accept: aVisitor [
	^ aVisitor visitPushLiteralVariable: self
]
