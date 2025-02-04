"
pop into temp
"
Class {
	#name : 'OCIRPopIntoTemp',
	#superclass : 'OCIRTempAccess',
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
OCIRPopIntoTemp >> accept: aVisitor [
	^ aVisitor visitPopIntoTemp: self
]

{ #category : 'accessing' }
OCIRPopIntoTemp >> nextBytecodeOffsetAfterJump [
	"if we are in to:do: answers the next byte code offset"
	^ self sequence last destination last destination first bytecodeOffset
]
