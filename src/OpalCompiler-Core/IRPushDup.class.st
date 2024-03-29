"
Instruction ""pushDup""
"
Class {
	#name : 'IRPushDup',
	#superclass : 'IRInstruction',
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
IRPushDup >> accept: aVisitor [
	^ aVisitor visitPushDup: self
]

{ #category : 'testing' }
IRPushDup >> isRemovableByPop [

	^ true
]
