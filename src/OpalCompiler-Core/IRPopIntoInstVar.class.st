"
Pop into instance variable.
"
Class {
	#name : #IRPopIntoInstVar,
	#superclass : #IRInstVarAccess,
	#category : 'OpalCompiler-Core-IR-Nodes'
}

{ #category : #visiting }
IRPopIntoInstVar >> accept: aVisitor [
	^ aVisitor visitPopIntoInstVar: self
]
