"
push temp
"
Class {
	#name : #IRPushTemp,
	#superclass : #IRTempAccess,
	#category : 'OpalCompiler-Core-IR-Nodes'
}

{ #category : #visiting }
IRPushTemp >> accept: aVisitor [
	^ aVisitor visitPushTemp: self
]
