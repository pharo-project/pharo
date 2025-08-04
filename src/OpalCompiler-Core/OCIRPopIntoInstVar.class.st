"
Pop into instance variable.
"
Class {
	#name : 'OCIRPopIntoInstVar',
	#superclass : 'OCIRInstVarAccess',
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
OCIRPopIntoInstVar >> accept: aVisitor [
	^ aVisitor visitPopIntoInstVar: self
]
