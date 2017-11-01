"
Instruction ""pushDup""
"
Class {
	#name : #IRPushDup,
	#superclass : #IRInstruction,
	#category : 'OpalCompiler-Core-IR-Nodes'
}

{ #category : #visiting }
IRPushDup >> accept: aVisitor [
	^ aVisitor visitPushDup: self
]
