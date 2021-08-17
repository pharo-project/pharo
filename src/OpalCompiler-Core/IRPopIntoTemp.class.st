"
pop into temp
"
Class {
	#name : #IRPopIntoTemp,
	#superclass : #IRTempAccess,
	#category : #'OpalCompiler-Core-IR-Nodes'
}

{ #category : #visiting }
IRPopIntoTemp >> accept: aVisitor [
	^ aVisitor visitPopIntoTemp: self
]

{ #category : #accessing }
IRPopIntoTemp >> nextBytecodeOffsetAfterJump [
	"if we are in to:do: answers the next byte code offset"
	^ self sequence last destination last destination first bytecodeOffset
]
