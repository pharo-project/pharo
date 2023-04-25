"
I model push thisThisProcess bytecode
"
Class {
	#name : #IRPushThisProcess,
	#superclass : #IRAccess,
	#category : #'OpalCompiler-Core-IR-Nodes'
}

{ #category : #visiting }
IRPushThisProcess >> accept: aVisitor [
	^ aVisitor visitPushThisProcess: self
]

{ #category : #testing }
IRPushThisProcess >> isRemovableByPop [

	^ true
]
