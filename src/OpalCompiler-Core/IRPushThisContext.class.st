"
I model push thisContext bytecode
"
Class {
	#name : #IRPushThisContext,
	#superclass : #IRAccess,
	#category : #'OpalCompiler-Core-IR-Nodes'
}

{ #category : #visiting }
IRPushThisContext >> accept: aVisitor [
	^ aVisitor visitPushThisContext: self
]

{ #category : #testing }
IRPushThisContext >> isRemovableByPop [

	^ true
]
