"
pop into literal variable
"
Class {
	#name : 'OCIRPopIntoLiteralVariable',
	#superclass : 'OCIRLiteralVariableAccess',
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
OCIRPopIntoLiteralVariable >> accept: aVisitor [
	^ aVisitor visitPopIntoLiteralVariable: self
]
