"
Instruction ""returnTop""
"
Class {
	#name : 'IRReturn',
	#superclass : 'IRInstruction',
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
IRReturn >> accept: aVisitor [
	^ aVisitor visitReturn: self
]

{ #category : 'testing' }
IRReturn >> isReturn [

	^ true
]

{ #category : 'accessing' }
IRReturn >> nonBodySuccessorSequences [
	^#()
]
