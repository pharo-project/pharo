"
Instruction ""popTop""
"
Class {
	#name : 'IRPop',
	#superclass : 'IRInstruction',
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
IRPop >> accept: aVisitor [
	^ aVisitor visitPop: self
]

{ #category : 'testing' }
IRPop >> isPop [
	^true
]
