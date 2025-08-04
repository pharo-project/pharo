"
push literal variable
"
Class {
	#name : 'OCIRPushLiteralVariable',
	#superclass : 'OCIRLiteralVariableAccess',
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
OCIRPushLiteralVariable >> accept: aVisitor [
	^ aVisitor visitPushLiteralVariable: self
]
