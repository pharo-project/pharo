"
Instruction ""popTop""
"
Class {
	#name : #IRPop,
	#superclass : #IRInstruction,
	#category : #'OpalCompiler-Core-IR-Nodes'
}

{ #category : #visiting }
IRPop >> accept: aVisitor [
	^ aVisitor visitPop: self
]

{ #category : #testing }
IRPop >> isPop [
	^true
]
