"
Instruction ""returnTop""
"
Class {
	#name : 'OCIRReturn',
	#superclass : 'OCIRInstruction',
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
OCIRReturn >> accept: aVisitor [
	^ aVisitor visitReturn: self
]

{ #category : 'testing' }
OCIRReturn >> isReturn [

	^ true
]

{ #category : 'accessing' }
OCIRReturn >> nonBodySuccessorSequences [
	^#()
]
